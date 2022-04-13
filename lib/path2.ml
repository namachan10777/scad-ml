open Vec
include Path.Make (Vec2)

type bbox =
  { min : Vec2.t
  ; max : Vec2.t
  }

let of_tups = List.map Vec2.of_tup
let of_path3 ?(plane = Plane.xy) = List.map (Plane.project plane)
let to_path3 ?(plane = Plane.xy) = List.map (Plane.lift plane)

let clockwise_sign' (ps : Vec2.t array) =
  let len = Array.length ps
  and sum = ref 0. in
  for i = 0 to len - 1 do
    let p1 = ps.(Util.index_wrap ~len i)
    and p2 = ps.(Util.index_wrap ~len (i + 1)) in
    sum := !sum +. ((p1.x -. p2.x) *. (p1.y +. p2.y))
  done;
  Float.(of_int @@ compare !sum 0.)

let is_clockwise' ps = Float.equal 1. (clockwise_sign' ps)
let clockwise_sign ps = clockwise_sign' (Array.of_list ps)
let is_clockwise ps = Float.equal 1. (clockwise_sign ps)

let self_intersections' ?(eps = Util.epsilon) path =
  let len = Array.length path in
  if len < 3
  then []
  else (
    let intersects = ref [] in
    for i = 0 to len - 3 do
      let l1 = Vec2.{ a = path.(i); b = path.(i + 1) } in
      let seg_normal =
        let d = Vec2.sub l1.b l1.a in
        Vec2.(normalize (v2 (-.d.y) d.x))
      in
      let vals = Array.map (fun p -> Vec2.dot p seg_normal) path
      and ref_v = Vec2.dot path.(i) seg_normal
      and last_signal = ref 0
      and start = i + 2 in
      for j = 0 to len - start - 1 do
        let v = vals.(j + start) -. ref_v in
        if Float.abs v >= eps
        then (
          let signal = Int.of_float @@ Math.sign v in
          if signal * !last_signal < 0
          then (
            let l2 = Vec2.{ a = path.(j + start); b = path.(j + start + 1) } in
            let intersect =
              Vec2.line_intersection ~bounds1:(true, true) ~bounds2:(true, true) l1 l2
            in
            Option.iter (fun p -> intersects := p :: !intersects) intersect );
          last_signal := signal )
      done
    done;
    !intersects )

let self_intersections ?eps path = self_intersections' ?eps (Array.of_list path)

let is_simple' ?eps ?(closed = false) path =
  let len = Array.length path in
  if len < 3
  then true
  else (
    let reversal = ref false
    and i = ref 0
    and last = len - if closed then 1 else 2 in
    while (not !reversal) && !i < last do
      let v1 = Vec2.sub path.(!i + 1) path.(!i)
      and v2 = Vec2.sub path.(Util.index_wrap ~len (!i + 2)) path.(!i + 1) in
      reversal := Math.approx Vec2.(dot v1 v2 /. norm v1 /. norm v2) (-1.);
      incr i
    done;
    if !reversal then false else List.length (self_intersections' ?eps path) = 0 )

let is_simple ?eps ?closed path = is_simple' ?eps ?closed (Array.of_list path)

let bbox = function
  | []       -> invalid_arg "Cannot calculate bbox for empty path."
  | hd :: tl ->
    let f bb { x; y } =
      let min = Float.{ x = min bb.min.x x; y = min bb.min.y y }
      and max = Float.{ x = max bb.max.x x; y = max bb.max.y y } in
      { min; max }
    in
    List.fold_left f { min = hd; max = hd } tl

let centroid ?(eps = Util.epsilon) = function
  | [] | [ _ ] | [ _; _ ] -> invalid_arg "Polygon must have more than two points."
  | p0 :: p1 :: tl        ->
    let f (area_sum, p_sum, p1) p2 =
      let { z = area; _ } = Vec2.(cross (sub p2 p0) (sub p1 p0)) in
      area +. area_sum, Vec2.(add p_sum (add p0 (add p1 p2))), p2
    in
    let area_sum, p_sum, _ = List.fold_left f (0., Vec2.zero, p1) tl in
    if Math.approx ~eps area_sum 0.
    then invalid_arg "The polygon is self-intersecting, or its points are collinear.";
    Vec2.(sdiv p_sum (area_sum *. 3.))

let area ?(signed = false) = function
  | [] | [ _ ] | [ _; _ ] -> 0.
  | p0 :: p1 :: tl        ->
    let f (area, p1) p2 =
      (area +. Vec2.(Vec3.get_z (cross (sub p1 p0) (sub p2 p0)))), p2
    in
    let area, _ = List.fold_left f (0., p1) tl in
    (if signed then area else Float.abs area) /. 2.

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

let lift plane = to_path3 ~plane
let translate p = List.map (Vec2.translate p)
let rotate r = List.map (Vec2.rotate r)
let rotate_about_pt r p = List.map (Vec2.rotate_about_pt r p)
let scale s = List.map (Vec2.scale s)
let mirror ax = List.map (Vec2.mirror ax)
let multmatrix m = List.map (fun { x; y } -> MultMatrix.transform m (v3 x y 0.))

let circle ?(fn = 30) r =
  let s = 2. *. Float.pi /. Float.of_int fn in
  let f i =
    let a = s *. Float.of_int i in
    v2 (r *. Float.cos a) (r *. Float.sin a)
  in
  List.init fn f

let square ?(center = false) { x; y } =
  if center
  then (
    let x' = x /. 2.
    and y' = y /. 2. in
    Vec2.[ v x' y'; v (-.x') y'; v (-.x') (-.y'); v x' (-.y') ] )
  else Vec2.[ v 0. 0.; v x 0.; v x y; v 0. y ]

let to_scad ?convexity t = Scad.polygon ?convexity t
