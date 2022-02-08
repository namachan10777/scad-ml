let shift_segment ~d (p1, p2) =
  let shift = Vec2.(add (mul_scalar (Vec2.line_normal p1 p2) d)) in
  shift p1, shift p2

(* Get the intersection point between two segments, or their common point if
   they already share one. *)
let segment_extension ((_, a2) as sa) ((b1, _) as sb) =
  if Vec2.(norm (sub a2 b1) < 1e-6) then a2 else Vec2.unbounded_intersection_exn sa sb

let chamfer ~centre ~delta p1 p2 p3 =
  let endline =
    let dist =
      let intersect = Vec2.unbounded_intersection_exn (p1, p3) (centre, p2) in
      Math.sign delta *. Vec2.(norm (centre <-> intersect))
    in
    shift_segment ~d:(delta -. dist) (p1, p3)
  in
  [ Vec2.unbounded_intersection_exn endline (p1, p2)
  ; Vec2.unbounded_intersection_exn endline (p2, p3)
  ]

(* If any part of a segment is further than distance d from the path (original
    path/outline before offseting). The number of points sampled along the
    segments for this approximation is set by quality. *)
let good_segments ?(quality = 1) ~closed ~d path shifted_segs =
  let len = Array.length path - if closed then 0 else 1
  and d = d -. 1e-7 in
  let path_segs =
    Array.init len (fun i -> Vec2.sub path.(Util.index_wrap ~len (i + 1)) path.(i))
  in
  let path_segs_norm = Array.map Vec2.norm path_segs in
  let path_segs_unit =
    Array.map2 (fun seg norm -> Vec2.div_scalar seg norm) path_segs path_segs_norm
  in
  let alphas =
    let q = Float.of_int quality +. 1. in
    let f = function
      | i when i = quality -> 0.
      | i when i = quality + 1 -> 1.
      | i -> (Float.of_int i +. 1.) /. q
    in
    Array.init (quality + 2) f
  and point_dist pt =
    let min = ref Float.max_float in
    for i = 0 to len - 1 do
      let v = Vec2.sub pt path.(i) in
      let proj = Vec2.dot v path_segs_unit.(i) in
      let seg_dist =
        if proj < 0.
        then Vec2.norm v
        else if proj > path_segs_norm.(i)
        then Vec2.(norm (sub pt path.(Util.index_wrap ~len (i + 1))))
        else Vec2.(norm (sub v (mul_scalar path_segs_unit.(i) proj)))
      in
      min := Float.min !min seg_dist
    done;
    !min
  in
  let f i =
    if i > len - 1
    then true
    else (
      let j = ref 0
      and good = ref false
      and p1, p2 = shifted_segs.(i) in
      while (not !good) && !j < quality + 2 do
        let a = alphas.(!j) in
        let pt = Vec2.(add (mul_scalar p1 a) (mul_scalar p2 (1. -. a))) in
        good := point_dist pt > d;
        incr j
      done;
      !good )
  in
  Array.init (Array.length shifted_segs) f

let offset' ?fn ?fs ?fa ?(closed = true) ?(check_valid = true) ?(quality = 1) offset path =
  let mode, d =
    let flip = if closed then Path2d.clockwise_sign path *. -1. else 1. in
    match offset with
    | `Delta d   -> `Delta, flip *. d
    | `Chamfer d -> `Chamfer, flip *. d
    | `Radius r  -> `Radius, flip *. r
  and path' = Array.of_list path in
  let len = Array.length path' in
  let shifted_segs =
    (* last looping segment ignored later if not closed *)
    let f i = shift_segment ~d (path'.(i), path'.(Util.index_wrap ~len (i + 1))) in
    List.init len f
  in
  let good =
    if check_valid
    then good_segments ~quality ~closed ~d path' (Array.of_list shifted_segs)
    else Array.make len true
  in
  if Array.for_all not good then raise (Failure "Offset of path is degenerate");
  let good_segs = List.filteri (fun i _ -> good.(i)) shifted_segs |> Array.of_list
  and good_path = List.filteri (fun i _ -> good.(i)) path |> Array.of_list in
  let len_good = Array.length good_segs in
  let sharp_corners =
    let f i =
      segment_extension good_segs.(Util.index_wrap ~len:len_good (i - 1)) good_segs.(i)
    in
    Array.init len_good f
  in
  let inside_corner =
    if Array.length sharp_corners = 2
    then [| false; false |]
    else (
      let f i =
        (* if path is open, ignore ends *)
        if (i = 0 || i = len_good - 1) && not closed
        then false
        else (
          let prev_a, prev_b = good_segs.(Util.index_wrap ~len:len_good (i - 1))
          and a, b = good_segs.(i)
          and c = sharp_corners.(i) in
          Vec2.(dot (sub b a) (sub a c)) > 0.
          && Vec2.(dot (sub prev_b prev_a) (sub c prev_b)) > 0. )
      in
      Array.init len_good f )
  in
  let new_corners, point_counts =
    let round i = inside_corner.(i) || ((not closed) && (i = 0 || i = len_good - 1)) in
    match mode with
    | `Delta   -> Array.to_list sharp_corners, List.init len_good (fun _ -> 1)
    | `Chamfer ->
      let f i =
        if round i
        then (
          let _, prev_b = good_segs.(Util.index_wrap ~len:len_good (i - 1))
          and a, _ = good_segs.(i) in
          chamfer ~delta:d ~centre:good_path.(i) prev_b sharp_corners.(i) a )
        else [ sharp_corners.(i) ]
      in
      let l = List.init len_good f in
      List.concat l, List.map List.length l
    | `Radius  ->
      let f i =
        if round i
        then (
          let _, prev_b = good_segs.(Util.index_wrap ~len:len_good (i - 1))
          and a, _ = good_segs.(i)
          and centre = good_path.(i) in
          let steps =
            let frags = Float.of_int @@ Util.helical_fragments ?fn ?fs ?fa d in
            let s =
              Float.(
                floor (frags *. Vec2.(angle (sub prev_b centre) (sub a centre)) /. pi))
            in
            Int.of_float (1. +. s)
          in
          if steps > 1
          then Path2d.arc_about_centre ~fn:steps ~centre prev_b a
          else [ sharp_corners.(i) ] )
        else [ sharp_corners.(i) ]
      in
      let l = List.init len_good f in
      List.concat l, List.map List.length l
  in
  good, new_corners, point_counts

let offset ?fn ?fs ?fa ?closed ?check_valid ?quality offset path =
  let _, points, _ = offset' ?fn ?fs ?fa ?closed ?check_valid ?quality offset path in
  points

let offset_with_faces
    ?fn
    ?fs
    ?fa
    ?closed
    ?check_valid
    ?quality
    ?(flip_faces = false)
    ?(start_idx = 0)
    offset
    path
  =
  let good, points, counts =
    offset' ?fn ?fs ?fa ?closed ?check_valid ?quality offset path
  in
  let counts = List.filteri (fun i _ -> good.(i)) counts in
  let n_first = List.length counts
  and n_second = List.fold_left ( + ) 0 counts in
  points
