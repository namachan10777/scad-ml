open Vec
include Path.Make (Vec2)
include Arc2
include Rounding.Make (Vec2) (Arc2)

let of_tups = List.map Vec2.of_tup
let of_path3 ?(plane = Plane.xy) = List.map (Plane.project plane)
let to_path3 ?(plane = Plane.xy) = List.map (Plane.lift plane)

let clockwise_sign ?(eps = Util.epsilon) = function
  | [] | [ _ ] | [ _; _ ] -> invalid_arg "Path/polygon must have more than two points."
  | p0 :: p1 :: t         ->
    let f (sum, last) p = sum +. ((last.x -. p.x) *. (last.y +. p.y)), p in
    let sum, _ = List.fold_left f (f (0., p0) p1) t in
    if Math.approx ~eps sum 0. then 0. else Float.(of_int @@ compare sum 0.)

let is_clockwise ps = Float.equal 1. (clockwise_sign ps)

let self_intersections ?eps ?closed path =
  APath2.self_intersections ?eps ?closed (Array.of_list path)

let is_simple ?eps ?closed path = APath2.is_simple ?eps ?closed (Array.of_list path)

let bbox = function
  | []       -> invalid_arg "Cannot calculate bbox for empty path."
  | hd :: tl ->
    let f (bb : Vec2.bbox) { x; y } =
      let min = Float.{ x = min bb.min.x x; y = min bb.min.y y }
      and max = Float.{ x = max bb.max.x x; y = max bb.max.y y } in
      Vec2.{ min; max }
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

let nearby_idxs ?(min_tree_size = 400) ?(radius = Util.epsilon) path =
  if List.length path < min_tree_size
  then
    fun target ->
    let g (i, idxs) p =
      if Vec2.approx ~eps:radius p target then i + 1, i :: idxs else i + 1, idxs
    in
    snd @@ List.fold_left g (0, []) path
  else (
    let tree = BallTree2.make path in
    BallTree2.search_idxs ~radius tree )

let nearby_points ?(min_tree_size = 400) ?(radius = Util.epsilon) path =
  if List.length path < min_tree_size
  then fun target -> List.filter (fun p -> Vec2.approx ~eps:radius p target) path
  else (
    let tree = BallTree2.make path in
    BallTree2.search_points ~radius tree )

let offset = Offset.offset
let lift plane = to_path3 ~plane
let translate p = List.map (Vec2.translate p)
let rotate r = List.map (Vec2.rotate r)
let rotate_about_pt r p = List.map (Vec2.rotate_about_pt r p)
let scale s = List.map (Vec2.scale s)
let mirror ax = List.map (Vec2.mirror ax)
let multmatrix m = List.map (fun { x; y } -> MultMatrix.transform m (v3 x y 0.))
let quaternion q = List.map (fun { x; y } -> Quaternion.rotate_vec3 q (v3 x y 0.))

let quaternion_about_pt q p =
  List.map (fun { x; y } -> Quaternion.rotate_vec3_about_pt q p (v3 x y 0.))

let vector_rotate ax r = quaternion (Quaternion.make ax r)
let vector_rotate_about_pt ax r = quaternion_about_pt (Quaternion.make ax r)

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
    Vec2.[ v x' (-.y'); v (-.x') (-.y'); v (-.x') y'; v x' y' ] )
  else Vec2.[ v 0. y; v x y; v x 0.; v 0. 0. ]

let star ~r1 ~r2 n =
  if n < 2 then invalid_arg "Cannot draw star path with less than 2 points.";
  let step = Float.(2. *. pi /. of_int n)
  and start_a = Float.pi /. -2.
  and pt r a = Float.(v2 (r *. cos a) (r *. sin a)) in
  let f i acc =
    let i = Float.of_int i in
    let a = start_a +. (step *. i) in
    pt r2 (a +. (step /. 2.)) :: pt r1 a :: acc
  in
  Util.fold_init n f []

let cubic_spline ?boundary ~fn ps = CubicSpline.(interpolate_path ~fn (fit ?boundary ps))

let show_points f t =
  Scad.union (List.mapi (fun i p -> Scad.translate (Vec3.of_vec2 p) (f i)) t)

let to_scad ?convexity t = Scad.polygon ?convexity t
