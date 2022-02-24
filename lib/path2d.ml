include Path.Make (Vec2)

type bounds =
  { left : float
  ; right : float
  ; top : float
  ; bot : float
  }

let clockwise_sign' ps =
  let len = Array.length ps
  and sum = ref 0. in
  for i = 0 to len - 1 do
    let x0, y0 = ps.(Util.index_wrap ~len i)
    and x1, y1 = ps.(Util.index_wrap ~len (i + 1)) in
    sum := !sum +. ((x0 -. x1) *. (y0 +. y1))
  done;
  Float.(of_int @@ compare !sum 0.)

let is_clockwise' ps = Float.equal 1. (clockwise_sign' ps)
let clockwise_sign ps = clockwise_sign' (Array.of_list ps)
let is_clockwise ps = Float.equal 1. (clockwise_sign ps)

let bounds = function
  | []           -> invalid_arg "Cannot calculate bounds for empty path."
  | (x, y) :: tl ->
    let f (left, right, top, bot) (x, y) =
      Float.(min left x, max right x, max top y, min bot y)
    in
    let left, right, top, bot = List.fold_left f (x, x, y, y) tl in
    { left; right; top; bot }

let self_intersections' ?(eps = Util.epsilon) path =
  let len = Array.length path in
  if len < 3
  then []
  else (
    let intersects = ref [] in
    for i = 0 to len - 3 do
      let l1 = Vec2.{ a = path.(i); b = path.(i + 1) } in
      let seg_normal =
        let x, y = Vec2.sub l1.b l1.a in
        Vec2.(normalize (-.y, x))
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

let centroid ?(eps = Util.epsilon) = function
  | [] | [ _ ] | [ _; _ ] -> invalid_arg "Polygon must have more than two points."
  | p0 :: p1 :: tl        ->
    let f (area_sum, p_sum, p1) p2 =
      let _, _, area = Vec2.(cross (sub p2 p0) (sub p1 p0)) in
      area +. area_sum, Vec2.(add p_sum (add p0 (add p1 p2))), p2
    in
    let area_sum, p_sum, _ = List.fold_left f (0., Vec2.zero, p1) tl in
    if Math.approx ~eps area_sum 0.
    then invalid_arg "The polygon is self-intersecting, or its points are collinear.";
    Vec2.(div_scalar p_sum (area_sum /. 3.))

let area ?(signed = false) = function
  | [] | [ _ ] | [ _; _ ] -> 0.
  | p0 :: p1 :: tl        ->
    let f (area, p1) p2 =
      (area +. Vec2.(Vec3.get_z (cross (sub p1 p0) (sub p2 p0)))), p2
    in
    let area, _ = List.fold_left f (0., p1) tl in
    if signed then area else Float.abs area

let arc ?(init = []) ?(rev = false) ?(fn = 10) ~centre:(cx, cy) ~radius ~start angle =
  let a_step = angle /. Float.of_int fn *. if rev then 1. else -1. in
  let f _ (acc, a) =
    ((Float.cos a *. radius) +. cx, (Float.sin a *. radius) +. cy) :: acc, a +. a_step
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
      invalid_arg "Co-linear points don't define unique arc. Must specify dir."
    | 0., Some `CW                  -> (2. *. Float.pi) -. a
    | 0., Some `CCW                 -> ((2. *. Float.pi) -. a) *. -1.
    | -1., Some `CW | 1., Some `CCW -> ((2. *. Float.pi) -. a) *. Float.neg d
    | _                             -> d *. a
  in
  arc ?init ?rev ?fn ~centre ~radius ~start angle

let arc_through ?init ?rev ?fn ((x1, y1) as p1) ((x2, y2) as p2) ((x3, y3) as p3) =
  if Vec2.collinear p1 p2 p3 then invalid_arg "Arc points must form a valid triangle.";
  let centre =
    let d = (2. *. (x1 -. x3) *. (y3 -. y2)) +. (2. *. (x2 -. x3) *. (y1 -. y3))
    and m1 = Vec2.dot p1 p1 -. Vec2.dot p3 p3
    and m2 = Vec2.dot p3 p3 -. Vec2.dot p2 p2 in
    let nx = (m1 *. (y3 -. y2)) +. (m2 *. (y3 -. y1))
    and ny = (m1 *. (x2 -. x3)) +. (m2 *. (x1 -. x3)) in
    nx /. d, ny /. d
  and dir = if Float.equal (Vec2.clockwise_sign p1 p2 p3) 1. then `CW else `CCW in
  arc_about_centre ?init ?rev ?fn ~dir ~centre p1 p3

let translate p = List.map (Vec2.translate p)
let rotate r = List.map (Vec2.rotate r)
let rotate_about_pt r p = List.map (Vec2.rotate_about_pt r p)
let scale s = List.map (Vec2.scale s)
let mirror ax = List.map (Vec2.mirror ax)
