(* NOTE: most of the functions in here will be pretty specialized so I think
    they'll be hidden by the interface, if not just included in the only place
    that turns out to use them.

   Top level functions:
     - polyRound, radiipoints -> Vec2.t list
     - polyRoundExtrude, radiipoints + params -> Scad.d3 (polyhedron)
     - beamChain (if I do it)
 *)

let index_wrap ~len i = ((i mod len) + len) mod len
let wrap_get a i = a.(index_wrap ~len:(Array.length a) i)

let clockwise_sign ps =
  let len = Array.length ps
  and sum = ref 0. in
  for i = 0 to len - 1 do
    let x0, y0, _ = ps.(index_wrap ~len i)
    and x1, y1, _ = ps.(index_wrap ~len (i + 1)) in
    sum := !sum +. ((x0 -. x1) *. (y0 +. y1))
  done;
  Float.(of_int @@ compare !sum 0.)

let cosine_rule_angle p1 p2 p3 =
  let d12 = Vec2.distance p1 p2
  and d13 = Vec2.distance p1 p3
  and d23 = Vec2.distance p2 p3 in
  Float.acos (((d23 *. d23) +. (d12 *. d12) -. (d13 *. d13)) /. (2. *. d23 *. d12))

let invtan run rise =
  let a = Float.(abs @@ atan (rise /. run)) in
  match Float.(compare rise 0., compare run 0.) with
  | 0, 1   -> 0.
  | 1, 1   -> a
  | 1, 0   -> Float.pi /. 2.
  | 1, -1  -> Float.pi -. a
  | 0, -1  -> Float.pi
  | -1, -1 -> Float.pi +. a
  | -1, 0  -> Float.pi *. 1.5
  | -1, 1  -> (2. *. Float.pi) -. a
  | _      -> failwith "PolyRound: inverse tan error, both args = 0."

let get_angle (x1, y1) (x2, y2) =
  if Float.(equal x1 x2 && equal y1 y2) then 0. else invtan (x2 -. x1) (y2 -. y1)

let get_gradient (x1, y1) (x2, y2) = (y2 -. y1) /. (x2 -. x1)

let parallel_follow
    ?(rmin = 1.)
    ~offset
    ~mode
    ((x1, y1, _) as rp1)
    ((x2, y2, r2) as rp2)
    ((x3, y3, _) as rp3)
  =
  if Float.equal offset 0.
  then x2, y2, 0. (* return middle point if offset is zero *)
  else (
    let p1 = x1, y1
    and p2 = x2, y2
    and p3 = x3, y3 in
    let path_angle = cosine_rule_angle p1 p2 p3
    and sign = clockwise_sign [| rp1; rp2; rp3 |] in
    let radius = mode *. sign *. offset /. Float.sin (path_angle /. 2.)
    and angle_to_centre =
      let mid_tangent =
        let tan_dist = offset /. Float.tan (path_angle /. 2.) in
        let tangent_point a b =
          let theta = get_angle a b in
          x2 -. (Float.cos theta *. tan_dist), y2 -. (Float.sin theta *. tan_dist)
        in
        Vec2.mean [ tangent_point p1 p2; tangent_point p2 p3 ]
      in
      get_angle mid_tangent p1
    in
    let cx = x2 -. (Float.cos angle_to_centre *. radius)
    and cy = y2 -. (Float.sin angle_to_centre *. radius)
    and r = Float.max rmin (r2 -. (offset *. sign *. mode)) in
    cx, cy, r )

let offset_poly ~offset rps =
  let cw_sign = Float.copy_sign offset 1. *. clockwise_sign rps *. -1.
  and len = Array.length rps in
  let out = Array.make len Vec3.zero in
  for i = 0 to len - 1 do
    out.(i)
      <- parallel_follow
           ~offset
           ~mode:cw_sign
           rps.(index_wrap ~len (i - 1))
           rps.(i)
           rps.(index_wrap ~len (i + 1))
  done;
  out

(* NOTE: findPoint is only ever used in beamChain *)
let intersect ?(r = 0.) a1 (x1, y1) a2 (x2, y2) =
  let is_vertical a = Float.(equal a (pi /. 2.) || equal a (pi *. 1.5)) in
  let m1 = Float.tan a1
  and m2 = Float.tan a2 in
  let c1 = y1 -. (m1 *. x1)
  and c2 = y2 -. (m2 *. x2)
  and a1_vert = is_vertical a1 in
  let x =
    if a1_vert then x1 else if is_vertical a2 then x2 else (c2 -. c1) /. (m1 -. m2)
  in
  let y = if a1_vert then (m2 *. x) +. c2 else (m1 *. x) +. c1 in
  x, y, r

(* NOTE: CenterN2PointArc is required by polyRound *)
let arc_about_centre
    ?(mode = `Shortest)
    ~fn
    ((x1, y1) as p1)
    ((x2, y2) as p2)
    ((cx, cy) as centre)
  =
  let sign = clockwise_sign [| cx, cy, 0.; x1, y1, 0.; x2, y2, 0. |] in
  let step_a =
    let path_angle = cosine_rule_angle p2 centre p1 in
    let arc_angle =
      match mode with
      | `Shortest -> path_angle
      | `Longest -> path_angle -. (2. *. Float.pi)
      | `CW when sign < 0. -> path_angle
      | `CW when sign > 0. -> path_angle -. (2. *. Float.pi)
      | `CCW when sign > 0. -> path_angle
      | `CCW when sign < 0. -> path_angle -. (2. *. Float.pi)
      | _ -> path_angle
    in
    arc_angle /. Float.of_int fn *. sign
  and start_a = get_angle centre p1
  and r = Vec2.distance p1 centre in
  let f i =
    let a = (step_a *. Float.of_int i) +. start_a in
    (r *. Float.cos a) +. cx, (r *. Float.sin a) +. cy
  in
  List.init (fn + 1) f

let round_5_points (x1, y1, _) (x2, y2, r2) (x3, y3, r3) (x4, y4, r4) (x5, y5, _) =
  let p1 = x1, y1
  and p2 = x2, y2
  and p3 = x3, y3
  and p4 = x4, y4
  and p5 = x5, y5 in
  let half_a2 = cosine_rule_angle p1 p2 p3 /. 2.
  and half_a3 = cosine_rule_angle p2 p3 p4 /. 2.
  and half_a4 = cosine_rule_angle p3 p4 p5 /. 2.
  and d23 = Vec2.distance p2 p3
  and d34 = Vec2.distance p3 p4 in
  let new_r =
    let open Float in
    let f23 =
      d23 *. tan half_a2 *. tan half_a3 /. ((r2 *. tan half_a3) +. (r3 *. tan half_a2))
    and f34 =
      d34 *. tan half_a3 *. tan half_a4 /. ((r3 *. tan half_a4) +. (r4 *. tan half_a3))
    in
    (* smallest radius after applying factors is selected to replace r3 *)
    min r3 (min (f23 *. r3) (f34 *. r3))
  in
  let tan_dist = new_r /. Float.tan half_a3 in
  let tangent_point a b =
    let theta = get_angle a b in
    x3 -. (Float.cos theta *. tan_dist), y3 -. (Float.sin theta *. tan_dist)
  in
  let t23 = tangent_point p2 p3
  and t34 = tangent_point p4 p3 in
  let centre =
    (* find centre with angle to mid point between tangents *)
    let radius = new_r /. Float.sin half_a3
    and angle = get_angle (Vec2.mean [ t23; t34 ]) p3 in
    x3 -. (Float.cos angle *. radius), y3 -. (Float.sin angle *. radius)
  in
  t23, t34, centre

let round_3_points (x1, y1, _) (x2, y2, r2) (x3, y3, _) =
  let p1 = x1, y1
  and p2 = x2, y2
  and p3 = x3, y3 in
  let path_angle = cosine_rule_angle p1 p2 p3 in
  let tan_dist = r2 /. Float.tan (path_angle /. 2.)
  and radius = r2 /. Float.sin (path_angle /. 2.) in
  let tangent_point a b =
    let theta = get_angle a b in
    x2 -. (Float.cos theta *. tan_dist), y2 -. (Float.sin theta *. tan_dist)
  in
  let t12 = tangent_point p1 p2
  and t23 = tangent_point p2 p3 in
  let centre =
    (* find centre with angle to mid point between tangents *)
    let angle = get_angle (Vec2.mean [ t12; t23 ]) p2 in
    x2 -. (Float.cos angle *. radius), y2 -. (Float.sin angle *. radius)
  in
  t12, t23, centre

(* NOTE: leave out process radii points and friends for now, it seems like it doesn't do
    anything unless there are addition flags set on the radiipoints (array
    element after the radius (4th dimension)). *)
(* NOTE: removed c-squared minus c-squared from under sqrt *)
(* let cosine_rule_bside a c c' =
 *   let cos_c' = Float.cos c' in
 *   (c *. cos_c') -. Float.sqrt ((a *. a) +. (cos_c' *. cos_c')) *)
(* let abs_a_rel_r (ox, oy, _) (nx, ny, nr) =
 *   let th2 = Float.atan (oy /. ox)
 *   and r2 = Float.sqrt ((ox *. ox) +. (oy *. oy)) in
 *   let r3 = cosine_rule_bside r2 ny (th2 -. nx) in
 *   Float.cos nx *. r3, Float.sin nx *. r3, nr *)
(* let process_radii_points rps =
 *   let rec aux final idx res =
 *     if idx >= final + 1
 *     then res
 *     else (
 *       let x, y, r = rps.(idx) in
 *       () )
 *   in
 *   () *)
let poly_round ?(rad_limit = true) ?(fn = 5) rps =
  let pruned =
    let rps = Array.of_list rps in
    let len = Array.length rps in
    let w = index_wrap ~len
    and ps = Array.map Vec2.of_vec3 rps in
    let rec aux i acc =
      if i < len
      then (
        let ((x, y, r) as rp) = rps.(i) in
        if (not (Poly.colinear ps.(w (i - 1)) (x, y) ps.(w (i + 1)))) || Float.equal 0. r
        then aux (i + 1) (rp :: acc)
        else aux (i + 1) acc )
      else acc
    in
    aux 0 [] |> List.rev |> Array.of_list
  in
  let len = Array.length pruned in
  let f =
    let get i = Array.unsafe_get pruned (index_wrap ~len i) in
    let round =
      if rad_limit
      then
        fun i ->
        round_5_points (get (i - 2)) (get (i - 1)) (get i) (get (i + 1)) (get (i + 2))
      else fun i -> round_3_points (get (i - 1)) (get i) (get (i + 1))
    in
    fun i ->
      let x, y, r = get i in
      if Float.equal r 0.
      then [ x, y ]
      else (
        let p1, p2, centre = round i in
        arc_about_centre ~fn p1 p2 centre )
  in
  List.flatten @@ List.init len f

(* NOTE: offsetAllFacesBy is only used in this function, iterating over all
    the faces of the bottom result from makeCurvedPartOfPolyHedron to increase
    the face indices by an offset. Since makeCurved... is going to be a local
    function in extrude_poly_w_radius (which is just extrude polyround
    basically), if should just give the offset for the bottom at the time of
    building. *)
let extrude_poly_w_radius = ()
