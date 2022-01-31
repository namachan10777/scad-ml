type t = Vec2.t list

let colinear = Path.colinear (module Vec2)
let total_travel' = Path.total_travel' (module Vec2)
let total_travel = Path.total_travel (module Vec2)
let cummulative_travel = Path.cummulative_travel (module Vec2)
let to_continuous = Path.to_continuous (module Vec2)
let resample ~freq = Path.resample (module Vec2) ~freq

let arc_through
    ?(init = [])
    ?(rev = false)
    ?(fn = 10)
    ((x1, y1) as p1)
    ((x2, y2) as p2)
    ((x3, y3) as p3)
  =
  if colinear p1 p2 p3 then failwith "Arc points must form a valid triangle.";
  let ((cx, cy) as centre) =
    let d = (2. *. (x1 -. x3) *. (y3 -. y2)) +. (2. *. (x2 -. x3) *. (y1 -. y3))
    and m1 = Vec2.dot p1 p1 -. Vec2.dot p3 p3
    and m2 = Vec2.dot p3 p3 -. Vec2.dot p2 p2 in
    let nx = (m1 *. (y3 -. y2)) +. (m2 *. (y3 -. y1))
    and ny = (m1 *. (x2 -. x3)) +. (m2 *. (x1 -. x3)) in
    nx /. d, ny /. d
  in
  let r = Vec2.distance centre p1
  and ((dx, dy) as v1) = Vec2.(p1 <-> centre)
  and v2 = Vec2.(p2 <-> centre)
  and v3 = Vec2.(p3 <-> centre) in
  let central_angle =
    Float.acos Vec2.(dot v1 v2 /. (norm v1 *. norm v2))
    +. Float.acos Vec2.(dot v2 v3 /. (norm v2 *. norm v3))
  in
  let start_a = Float.atan2 dy dx
  and step_a = central_angle /. Float.of_int fn *. if rev then 1. else -1. in
  let rec loop acc i a =
    if i <= fn
    then
      loop
        (((Float.cos a *. r) +. cx, (Float.sin a *. r) +. cy) :: acc)
        (i + 1)
        (a +. step_a)
    else acc
  in
  loop init 0 (if rev then start_a else start_a +. central_angle)

let translate p = List.map (Vec2.translate p)
let rotate r = List.map (Vec2.rotate r)
let rotate_about_pt r p = List.map (Vec2.rotate_about_pt r p)
let scale s = List.map (Vec2.scale s)
let mirror ax = List.map (Vec2.mirror ax)
