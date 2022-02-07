include Path.Make (Vec2)

let arc ?(init = []) ?(rev = false) ?(fn = 10) ~centre:(cx, cy) ~radius ~start angle =
  let step_a = angle /. Float.of_int fn *. if rev then 1. else -1. in
  let f _ (acc, a) =
    ((Float.cos a *. radius) +. cx, (Float.sin a *. radius) +. cy) :: acc, a +. step_a
  in
  fst @@ Util.fold_init (fn + 1) f (init, if rev then start else start +. angle)

let arc_about_centre ?init ?rev ?fn ?dir ~centre p1 p2 =
  let radius = Vec2.distance centre p1
  and start =
    let dx, dy = Vec2.sub p1 centre in
    Float.atan2 dy dx
  and angle =
    let a = Vec2.angle_points p1 centre p2
    and d = Vec2.clockwise_sign p1 p2 centre in
    match d, dir with
    | 0., None                      ->
      raise
        (Invalid_argument "Co-linear points don't define unique arc. Must specify dir.")
    | 0., Some `CW                  -> ((2. *. Float.pi) -. a) *. -1.
    | 0., Some `CCW                 -> (2. *. Float.pi) -. a
    | 1., Some `CW | -1., Some `CCW -> ((2. *. Float.pi) -. a) *. Float.neg d
    | _                             -> d *. a
  in
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
  and dir = if Float.equal (Vec2.clockwise_sign p1 p2 p3) 1. then `CCW else `CW in
  arc_about_centre ?init ?rev ?fn ~dir ~centre p1 p3

let translate p = List.map (Vec2.translate p)
let rotate r = List.map (Vec2.rotate r)
let rotate_about_pt r p = List.map (Vec2.rotate_about_pt r p)
let scale s = List.map (Vec2.scale s)
let mirror ax = List.map (Vec2.mirror ax)
