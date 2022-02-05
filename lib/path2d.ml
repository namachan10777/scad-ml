include Path.Make (Vec2)

let arc ?(init = []) ?(rev = false) ?(fn = 10) ~centre:(cx, cy) ~radius ~start angle =
  let step_a = angle /. Float.of_int fn *. if rev then 1. else -1. in
  let rec loop acc i a =
    if i <= fn
    then
      loop
        (((Float.cos a *. radius) +. cx, (Float.sin a *. radius) +. cy) :: acc)
        (i + 1)
        (a +. step_a)
    else acc
  in
  loop init 0 (if rev then start else start +. angle)

let arc_about_centre ?init ?rev ?fn ~centre p1 p2 =
  let radius = Vec2.distance centre p1
  and start =
    let dx, dy = Vec2.sub p1 centre in
    Float.atan2 dy dx
  and angle = Vec2.angle_points p1 centre p2 in
  arc ?init ?rev ?fn ~centre ~radius ~start angle

let arc_through ?init ?rev ?fn ((x1, y1) as p1) ((x2, y2) as p2) ((x3, y3) as p3) =
  if Vec2.colinear p1 p2 p3
  then raise (Invalid_argument "Arc points must form a valid triangle.");
  let centre =
    let d = (2. *. (x1 -. x3) *. (y3 -. y2)) +. (2. *. (x2 -. x3) *. (y1 -. y3))
    and m1 = Vec2.dot p1 p1 -. Vec2.dot p3 p3
    and m2 = Vec2.dot p3 p3 -. Vec2.dot p2 p2 in
    let nx = (m1 *. (y3 -. y2)) +. (m2 *. (y3 -. y1))
    and ny = (m1 *. (x2 -. x3)) +. (m2 *. (x1 -. x3)) in
    nx /. d, ny /. d
  in
  let radius = Vec2.distance centre p1
  and ((dx, dy) as v1) = Vec2.(p1 <-> centre)
  and v2 = Vec2.(p2 <-> centre)
  and v3 = Vec2.(p3 <-> centre) in
  let angle =
    Float.acos Vec2.(dot v1 v2 /. (norm v1 *. norm v2))
    +. Float.acos Vec2.(dot v2 v3 /. (norm v2 *. norm v3))
  in
  let start = Float.atan2 dy dx in
  arc ?init ?rev ?fn ~centre ~radius ~start angle

let translate p = List.map (Vec2.translate p)
let rotate r = List.map (Vec2.rotate r)
let rotate_about_pt r p = List.map (Vec2.rotate_about_pt r p)
let scale s = List.map (Vec2.scale s)
let mirror ax = List.map (Vec2.mirror ax)
