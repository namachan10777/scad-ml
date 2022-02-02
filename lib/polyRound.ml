open Util

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
  | _      -> raise (Invalid_argument "Run and rise cannot both = 0.")

let get_angle (x1, y1) (x2, y2) =
  if Float.(equal x1 x2 && equal y1 y2) then 0. else invtan (x2 -. x1) (y2 -. y1)

(* TODO: consider a check that notices when the shape implodes due to too great
    of offset. This can lead to extrude errors that may be confusing. *)
let offset_poly ~offset ps =
  let cw_sign = Math.sign offset *. Poly2d.clockwise_sign' ps *. -1.
  and len = Array.length ps in
  let parallel_follow (x1, y1) (x2, y2) (x3, y3) =
    if Float.equal offset 0.
    then x2, y2 (* return middle point if offset is zero *)
    else (
      let p1 = x1, y1
      and p2 = x2, y2
      and p3 = x3, y3 in
      let path_angle = Vec2.angle_points p1 p2 p3
      and local_sign = Poly2d.clockwise_sign' [| p1; p2; p3 |] in
      let radius = cw_sign *. local_sign *. offset /. Float.sin (path_angle /. 2.)
      and angle_to_centre =
        let mid_tangent =
          let tan_dist = offset /. Float.tan (path_angle /. 2.) in
          let tangent_point a b =
            let theta = get_angle a b in
            x2 -. (Float.cos theta *. tan_dist), y2 -. (Float.sin theta *. tan_dist)
          in
          Vec2.mean [ tangent_point p1 p2; tangent_point p3 p2 ]
        in
        get_angle mid_tangent p2
      in
      let cx = x2 -. (Float.cos angle_to_centre *. radius)
      and cy = y2 -. (Float.sin angle_to_centre *. radius) in
      cx, cy )
  and out = Array.make len Vec2.zero in
  for i = 0 to len - 1 do
    out.(i)
      <- parallel_follow ps.(index_wrap ~len (i - 1)) ps.(i) ps.(index_wrap ~len (i + 1))
  done;
  out

let arc_about_centre ?(mode = `Shortest) ~fn p1 p2 ((cx, cy) as centre) =
  let sign = Poly2d.clockwise_sign' [| centre; p1; p2 |] in
  let step_a =
    let path_angle = Vec2.angle_points p2 centre p1 in
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
  let half_a2 = Vec2.angle_points p1 p2 p3 /. 2.
  and half_a3 = Vec2.angle_points p2 p3 p4 /. 2.
  and half_a4 = Vec2.angle_points p3 p4 p5 /. 2.
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
  let path_angle = Vec2.angle_points p1 p2 p3 in
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

let prune_radii_points rps =
  let len = Array.length rps in
  let w = index_wrap ~len
  and ps = Array.map Vec2.of_vec3 rps in
  let rec aux i acc =
    if i < len
    then (
      let ((x, y, r) as rp) = rps.(i) in
      if (not (Vec2.colinear ps.(w (i - 1)) (x, y) ps.(w (i + 1)))) || Float.equal 0. r
      then aux (i + 1) (rp :: acc)
      else aux (i + 1) acc )
    else acc
  in
  aux 0 [] |> List.rev |> Array.of_list

let polyround' ?(rad_limit = true) ?(fn = 5) rps =
  let pruned = prune_radii_points rps in
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

let polyround_sweep ?(min_r = 0.01) ?(fn = 4) ?cap_fn ~transforms ~r1 ~r2 rps =
  let rps = Array.of_list rps
  and cap_fn = Option.value ~default:fn cap_fn in
  let len = Array.length rps
  and frac_step = 1. /. Float.of_int cap_fn in
  let ps = Array.make len Vec2.zero
  and radii = Array.make len 0. in
  for i = 0 to len - 1 do
    let x, y, r = Array.unsafe_get rps i in
    Array.unsafe_set ps i (x, y);
    Array.unsafe_set radii i r
  done;
  (* Ensure polygon is counter-clockwise to satisfy polyhedron assumptions. *)
  if Float.equal (Poly2d.clockwise_sign' ps) 1.
  then (
    rev_array rps;
    rev_array ps;
    rev_array radii );
  let shape = List.map Vec3.of_vec2 @@ polyround' ~fn rps
  and cap_points ?(init = []) top m =
    let r, z_sign = if top then r2, 1. else r1, -1. in
    let dir = Float.(of_int @@ compare r 0.)
    and abs_r = Float.abs r in
    let layer i =
      let step = Float.of_int i *. frac_step *. abs_r in
      let offset = (dir *. Float.sqrt ((r *. r) -. (step *. step))) -. (dir *. abs_r) in
      let get_op idx = Array.unsafe_get (offset_poly ~offset ps) (index_wrap ~len idx) in
      let adjust_radii j =
        let local_sign =
          Poly2d.clockwise_sign' [| get_op (j - 1); get_op j; get_op (j + 1) |]
        in
        let x, y = get_op j in
        (* The overall polygon rotation is enforced to be CCW, so if the local
               sign is positive (CW), subtract from the radius instead *)
        let r' = Float.max min_r (radii.(j) +. (offset *. local_sign *. -1.)) in
        x, y, r'
      and z = z_sign *. step in
      Array.init len adjust_radii
      |> polyround' ~fn
      |> List.map (fun p -> MultMatrix.transform m @@ Vec2.to_vec3 ~z p)
    and idx = if top then Fun.id else ( - ) (cap_fn + 1) in
    let rec loop acc i =
      if i < cap_fn + 1 then loop (layer (idx i) :: acc) (i + 1) else acc
    in
    loop init 1
  in
  let layers =
    match transforms with
    | []      -> cap_points
                   ~init:(shape :: cap_points false MultMatrix.id)
                   true
                   MultMatrix.id
    | hd :: _ ->
      let init, last_transform =
        let f (acc, _last) m = List.map (MultMatrix.transform m) shape :: acc, m in
        List.fold_left f (cap_points false hd, hd) transforms
      in
      cap_points ~init true last_transform
  in
  Poly3d.of_layers (List.rev layers)

let polyround_extrude
    ?min_r
    ?fn
    ?cap_fn
    ?slices
    ?scale
    ?(twist = 0.)
    ?(center = false)
    ~height
    ~r1
    ~r2
    rps
  =
  let slices = helical_slices ?fn:slices twist in
  let bot = if center then height /. -2. else Float.abs r1
  and s = height /. Float.of_int slices
  and twist = if Float.abs twist > 0. then Some twist else None in
  let transforms =
    List.init (slices + 1) (fun i -> 0., 0., (Float.of_int i *. s) +. bot)
    |> Path3d.to_transforms ?scale ?twist
  in
  polyround_sweep ?min_r ?fn ?cap_fn ~r1 ~r2 ~transforms rps

let polyround ?rad_limit ?fn rps = polyround' ?rad_limit ?fn (Array.of_list rps)
