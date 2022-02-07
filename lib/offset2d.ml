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
      let intersect = Vec2.unbounded_intersection_exn (p3, p1) (centre, p2) in
      Math.sign delta *. Vec2.(norm (centre <-> intersect))
    in
    shift_segment ~d:(delta -. dist) (p1, p3)
  in
  [ Vec2.unbounded_intersection_exn endline (p1, p2)
  ; Vec2.unbounded_intersection_exn endline (p2, p3)
  ]

let good_segments ~quality ~closed ~d path shifted_segs =
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
        good := point_dist pt > d
      done;
      !good )
  in
  List.init (Array.length shifted_segs) f
