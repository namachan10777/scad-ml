let circle ?(fn = 30) r =
  let s = 2. *. Float.pi /. Float.of_int fn in
  let f i =
    let a = s *. Float.of_int i in
    [ r *. Float.cos a; r *. Float.sin a ]
  in
  List.init fn f

let square ?(center = false) (x, y) =
  if center
  then (
    let x' = x /. 2.
    and y' = y /. 2. in
    [ x', y'; -.x', y'; -.x', -.y'; x', -.y' ] )
  else [ 0., 0.; x, 0.; x, y; 0., y ]

let arc
    ?(init = [])
    ?(rev = false)
    ?(fn = 10)
    ((x1, y1) as p1)
    ((x2, y2) as p2)
    ((x3, y3) as p3)
  =
  if let a = Vec2.distance p1 p2
     and b = Vec2.distance p2 p3
     and c = Vec2.distance p3 p1 in
     a +. b < c || b +. c < a || c +. a < b
  then failwith "Arc points must form a valid triangle.";
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
  and step_a = central_angle /. Float.of_int (fn - 1) *. if rev then 1. else -1. in
  let rec loop acc i a =
    if i < fn
    then
      loop
        (((Float.cos a *. r) +. cx, (Float.sin a *. r) +. cy) :: acc)
        (i + 1)
        (a +. step_a)
    else acc
  in
  loop init 0 (if rev then start_a else start_a +. central_angle)
