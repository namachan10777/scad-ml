open Vec

let arc
    ?(rev = false)
    ?(fn = 16)
    ?(wedge = false)
    ~centre:(c : Vec2.t)
    ~radius
    ~start
    angle
  =
  let a_step = angle /. Float.of_int fn *. if rev then 1. else -1. in
  let f _ (acc, a) =
    let p = v2 ((Float.cos a *. radius) +. c.x) ((Float.sin a *. radius) +. c.y) in
    p :: acc, a +. a_step
  and init = if wedge then [ c ] else [] in
  fst @@ Util.fold_init (fn + 1) f (init, if rev then start else start +. angle)

let arc_about_centre ?rev ?fn ?dir ?wedge ~centre p1 p2 =
  let radius = Vec2.distance centre p1
  and start =
    let { x; y } = Vec2.sub p1 centre in
    Float.atan2 y x
  and angle =
    let a = Vec2.angle_points p1 centre p2
    and d = Vec2.clockwise_sign p1 p2 centre in
    match d, dir with
    | 0., None                      ->
      invalid_arg "Co-linear points don't define unique arc. Must specify dir."
    | 0., Some `CW                  -> (2. *. Float.pi) -. a
    | 0., Some `CCW                 -> ((2. *. Float.pi) -. a) *. -1.
    | -1., Some `CW | 1., Some `CCW -> ((2. *. Float.pi) -. a) *. Float.neg d
    | _                             -> d *. a
  in
  arc ?rev ?fn ?wedge ~centre ~radius ~start angle

let arc_through ?rev ?fn ?wedge p1 p2 p3 =
  if Vec2.collinear p1 p2 p3 then invalid_arg "Arc points must form a valid triangle.";
  let centre =
    let d =
      (2. *. (p1.x -. p3.x) *. (p3.y -. p2.y)) +. (2. *. (p2.x -. p3.x) *. (p1.y -. p3.y))
    and m1 = Vec2.dot p1 p1 -. Vec2.dot p3 p3
    and m2 = Vec2.dot p3 p3 -. Vec2.dot p2 p2 in
    let nx = (m1 *. (p3.y -. p2.y)) +. (m2 *. (p3.y -. p1.y))
    and ny = (m1 *. (p2.x -. p3.x)) +. (m2 *. (p1.x -. p3.x)) in
    v2 (nx /. d) (ny /. d)
  and dir = if Float.equal (Vec2.clockwise_sign p1 p2 p3) 1. then `CW else `CCW in
  arc_about_centre ?rev ?fn ?wedge ~dir ~centre p1 p3
