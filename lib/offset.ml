let shift_segment ~d Vec2.{ a; b } =
  let shift = Vec2.(add (smul (Vec2.line_normal a b) d)) in
  Vec2.{ a = shift a; b = shift b }

(* Get the intersection point between two segments, or their common point if
   they already share one. *)
let segment_extension (Vec2.{ b = b1; _ } as s1) (Vec2.{ a = a2; _ } as s2) =
  if Vec2.(norm (sub b1 a2) < 1e-6)
  then a2
  else (
    match Vec2.line_intersection s1 s2 with
    | Some inter -> inter
    | None       -> failwith "Offset: path contains segment that reverses direction." )

let chamfer ~centre ~delta p1 p2 p3 =
  let endline =
    let seg = Vec2.{ a = p1; b = p3 } in
    let dist =
      let intersect =
        match Vec2.(line_intersection seg { a = centre; b = p2 }) with
        | Some p -> p
        | None   -> failwith "Offset: chamfer centre line is parallel (no intersect)"
      in
      Math.sign delta *. Vec2.(norm (centre -@ intersect))
    in
    shift_segment ~d:(delta -. dist) seg
  in
  (* if endline segment is colinear with either input segment, return middle *)
  match
    Vec2.(
      ( line_intersection endline { a = p1; b = p2 }
      , line_intersection endline { a = p2; b = p3 } ))
  with
  | Some i1, Some i2 -> [ i1; i2 ]
  | _                -> [ p2 ]

(* If any part of a segment is further than distance d from the path (original
    path/outline before offseting). The number of points sampled along the
    segments for this approximation is set by quality. *)
let good_segments ~quality ~closed ~d path shifted_segs =
  let len = Array.length path - if closed then 0 else 1
  and d = d -. 1e-7 in
  let path_segs =
    Array.init len (fun i -> Vec2.sub path.(Util.index_wrap ~len (i + 1)) path.(i))
  in
  let path_segs_norm = Array.map Vec2.norm path_segs in
  let path_segs_unit =
    Array.map2 (fun seg norm -> Vec2.sdiv seg norm) path_segs path_segs_norm
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
      let vec = Vec2.sub pt path.(i) in
      let proj = Vec2.dot vec path_segs_unit.(i) in
      let seg_dist =
        if proj < 0.
        then Vec2.norm vec
        else if proj > path_segs_norm.(i)
        then Vec2.(norm (sub pt path.(Util.index_wrap ~len (i + 1))))
        else Vec2.(norm (sub vec (smul path_segs_unit.(i) proj)))
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
      and Vec2.{ a = ssa; b = ssb } = shifted_segs.(i) in
      while (not !good) && !j < quality + 2 do
        let a = alphas.(!j) in
        let pt = Vec2.(add (smul ssa a) (smul ssb (1. -. a))) in
        good := point_dist pt > d;
        incr j
      done;
      !good )
  in
  Array.init (Array.length shifted_segs) f

let offset' ?fn ?fs ?fa ?(closed = true) ?(check_valid = Some 1) offset path =
  let path = Array.of_list path in
  let mode, d =
    let flip = if closed then Path2.clockwise_sign' path *. -1. else 1. in
    match offset with
    | `Delta d   -> `Delta, flip *. d
    | `Chamfer d -> `Chamfer, flip *. d
    | `Radius r  -> `Radius, flip *. r
  and len = Array.length path in
  let shifted_segs =
    (* last looping segment ignored later if not closed *)
    let f i =
      shift_segment ~d Vec2.{ a = path.(i); b = path.(Util.index_wrap ~len (i + 1)) }
    in
    Array.init len f
  in
  let good =
    match check_valid with
    | Some quality -> good_segments ~quality ~closed ~d path shifted_segs
    | None         -> Array.make len true
  in
  let n_good = Array.fold_left (fun sum b -> Bool.to_int b + sum) 0 good in
  if n_good = 0 then failwith "Offset of path is degenerate";
  let good_segs = Array.make n_good Vec2.{ a = zero; b = zero }
  and good_path = Array.make n_good Vec2.zero in
  let () =
    let idx = ref 0 in
    for i = 0 to len - 1 do
      if good.(i)
      then (
        good_segs.(!idx) <- shifted_segs.(i);
        good_path.(!idx) <- path.(i);
        incr idx )
    done
  in
  let sharp_corners =
    let f i =
      segment_extension good_segs.(Util.index_wrap ~len:n_good (i - 1)) good_segs.(i)
    in
    Array.init n_good f
  in
  let inside_corner =
    if Array.length sharp_corners = 2
    then [| false; false |]
    else (
      let f i =
        (* if path is open, ignore ends *)
        if (i = 0 || i = n_good - 1) && not closed
        then false
        else (
          let Vec2.{ a = prev_a; b = prev_b } =
            good_segs.(Util.index_wrap ~len:n_good (i - 1))
          and Vec2.{ a; b } = good_segs.(i)
          and c = sharp_corners.(i) in
          Vec2.(dot (sub b a) (sub a c)) > 0.
          && Vec2.(dot (sub prev_b prev_a) (sub c prev_b)) > 0. )
      in
      Array.init n_good f )
  in
  let new_corners, point_counts =
    let round i = inside_corner.(i) || ((not closed) && (i = 0 || i = n_good - 1)) in
    match mode with
    | `Delta   -> Array.to_list sharp_corners, List.init n_good (fun _ -> 1)
    | `Chamfer ->
      let f i =
        if round i
        then (
          let Vec2.{ b = prev_b; _ } = good_segs.(Util.index_wrap ~len:n_good (i - 1))
          and Vec2.{ a; _ } = good_segs.(i) in
          chamfer ~delta:d ~centre:good_path.(i) prev_b sharp_corners.(i) a )
        else [ sharp_corners.(i) ]
      in
      let l = List.init n_good f in
      List.concat l, List.map List.length l
    | `Radius  ->
      let f i =
        if round i
        then (
          let Vec2.{ b = prev_b; _ } = good_segs.(Util.index_wrap ~len:n_good (i - 1))
          and Vec2.{ a; _ } = good_segs.(i)
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
          then Path2.arc_about_centre ~fn:steps ~centre prev_b a
          else [ sharp_corners.(i) ] )
        else [ sharp_corners.(i) ]
      in
      let l = List.init n_good f in
      List.concat l, List.map List.length l
  in
  good, new_corners, point_counts

let offset ?fn ?fs ?fa ?closed ?check_valid offset path =
  let _, points, _ = offset' ?fn ?fs ?fa ?closed ?check_valid offset path in
  points

let offset_with_faces
    ?fn
    ?fs
    ?fa
    ?(closed = true)
    ?check_valid
    ?(flip_faces = false)
    ?(start_idx = 0)
    offset
    path
  =
  let good, points, counts = offset' ?fn ?fs ?fa ~closed ?check_valid offset path in
  let len = Array.length good in
  (* Expand the point count list with zeros where points from the original path
      were lost (essentially a lookup of how many points are on the offset path
      for each point on the original path). *)
  let counts =
    let cs = Array.of_list counts
    and expanded = Array.make len 0
    and j = ref 0 in
    for i = 0 to len - 1 do
      if good.(i)
      then (
        expanded.(i) <- cs.(!j);
        incr j )
    done;
    expanded
  in
  let n_first = len
  and n_second = Array.fold_left ( + ) 0 counts
  and start_i = start_idx
  and start_j = start_idx + len
  and j = ref 0
  and faces = ref [] in
  let add_face =
    if flip_faces
    then fun a -> faces := List.rev a :: !faces
    else fun a -> faces := a :: !faces
  in
  for i = 0 to n_first - Bool.to_int (not closed) - 1 do
    let c = counts.(i)
    and j' = !j in
    if c = 0
    then
      (* Point in original path no longer exists in the offset path. Thus a
           triangular face is made from the adjacent points on th original, to the
           current point in the new path. *)
      add_face
        [ (j' mod n_second) + start_j; i + start_i; ((i + 1) mod n_first) + start_i ]
    else
      (* Triangular faces for the extra points in the offset path, if there are any. *)
      for k = 0 to c - 2 do
        add_face [ i + start_i; j' + k + 1 + start_j; j' + k + start_j ]
      done;
    (* Quadrilateral face *)
    add_face
      [ i + start_i
      ; ((i + 1) mod n_first) + start_i
      ; ((j' + c) mod n_second) + start_j
      ; ((j' + c - 1) mod n_second) + start_j
      ];
    j := j' + c
  done;
  n_second, points, !faces
