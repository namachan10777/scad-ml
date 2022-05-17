open Vec
include Path.Make (Vec2)
include Arc2
include Rounding.Make (Vec2) (Arc2)

type bbox =
  { min : Vec2.t
  ; max : Vec2.t
  }

let of_tups = List.map Vec2.of_tup
let of_path3 ?(plane = Plane.xy) = List.map (Plane.project plane)
let to_path3 ?(plane = Plane.xy) = List.map (Plane.lift plane)
let clockwise_sign ?eps ps = APath2.clockwise_sign ?eps (Array.of_list ps)
let is_clockwise ps = Float.equal 1. (clockwise_sign ps)

let self_intersections ?eps ?closed path =
  APath2.self_intersections ?eps ?closed (Array.of_list path)

let is_simple ?eps ?closed path = APath2.is_simple ?eps ?closed (Array.of_list path)

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

let point_inside ?(eps = Util.epsilon) ?(nonzero = false) t p =
  let bb = bbox t in
  if p.x < bb.min.x -. eps
     || p.x > bb.max.x +. eps
     || p.y < bb.min.y -. eps
     || p.y > bb.max.y +. eps
  then `Outside
  else (
    let segs = segment ~closed:true t in
    let exception OnBorder in
    if try
         let f (s : Vec2.line) =
           if Vec2.distance s.a s.b > eps
              && Vec2.point_on_line ~eps ~bounds:(true, true) ~line:s p
           then raise OnBorder
         in
         List.iter f segs;
         false
       with
       | OnBorder -> true
    then `OnBorder
    else if nonzero
    then (
      let f sum (s : Vec2.line) =
        let p0 = Vec2.sub s.a p
        and p1 = Vec2.sub s.b p in
        let w =
          if Vec2.distance p0 p1 > eps
          then (
            let c = (Vec2.cross p0 (Vec2.sub p1 p0)).z in
            if p0.y <= 0.
            then if p1.y > 0. && c > 0. then 1 else 0
            else if p1.y <= 0. && c < 0.
            then -1
            else 0 )
          else 0
        in
        w + sum
      in
      if List.fold_left f 0 segs <> 0 then `Inside else `Outside )
    else (
      let f crossings (s : Vec2.line) =
        let p0 = Vec2.sub s.a p
        and p1 = Vec2.sub s.b p in
        if ((p1.y > eps && p0.y <= eps) || (p1.y <= eps && p0.y > eps))
           && -.eps < p0.x -. (p0.y *. (p1.x -. p0.x) /. (p1.y -. p0.y))
        then crossings + 1
        else crossings
      in
      if (2 * (List.fold_left f 0 segs mod 2)) - 1 > 0 then `Inside else `Outside ) )

let offset = Offset.offset
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

let wedge ?fn ~centre ~radius ~start angle =
  arc ?fn ~wedge:true ~centre ~radius ~start angle

let square ?(center = false) { x; y } =
  if center
  then (
    let x' = x /. 2.
    and y' = y /. 2. in
    Vec2.[ v x' y'; v (-.x') y'; v (-.x') (-.y'); v x' (-.y') ] )
  else Vec2.[ v 0. 0.; v x 0.; v x y; v 0. y ]

let to_scad ?convexity t = Scad.polygon ?convexity t
