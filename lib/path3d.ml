open Vec
include Path.Make (Vec3)

type bbox =
  { min : Vec3.t
  ; max : Vec3.t
  }

let of_tups = List.map Vec3.of_tup
let of_path2 ?z = List.map (Vec3.of_vec2 ?z)
let to_path2 = List.map Vec3.to_vec2

let bbox = function
  | []       -> invalid_arg "Cannot calculate bbox for empty path."
  | hd :: tl ->
    let f bb { x; y; z } =
      let min = Float.{ x = min bb.min.x x; y = min bb.min.y y; z = min bb.min.z z }
      and max = Float.{ x = max bb.max.x x; y = max bb.max.y y; z = max bb.max.z z } in
      { min; max }
    in
    List.fold_left f { min = hd; max = hd } tl

let arc ?init ?rev ?fn ~centre ~radius ~start angle =
  let arc =
    Path2d.arc ?rev ?fn ~centre:(Vec3.to_vec2 centre) ~radius ~start angle
    |> List.map (Vec3.of_vec2 ~z:(Vec3.get_z centre))
  in
  match init with
  | Some init -> List.concat [ arc; init ]
  | None      -> arc

let arc_about_centre ?init ?rev ?fn ?dir ~centre p1 p2 =
  let plane = Plane.make centre p1 p2 in
  let project = Plane.project plane
  and lift = List.map (Plane.lift plane) in
  let p1' = project p1
  and p2' = project p2
  and centre' = project centre in
  let arc = lift @@ Path2d.arc_about_centre ?rev ?dir ?fn ~centre:centre' p1' p2' in
  match init with
  | Some init -> List.concat [ arc; init ]
  | None      -> arc

let arc_through ?init ?rev ?fn p1 p2 p3 =
  let plane = Plane.make p3 p1 p2 in
  let project = Plane.project plane
  and lift = List.map (Plane.lift plane) in
  let p1' = project p1
  and p2' = project p2
  and p3' = project p3 in
  let arc = lift @@ Path2d.arc_through ?rev ?fn p1' p2' p3' in
  match init with
  | Some init -> List.concat [ arc; init ]
  | None      -> arc

let helix ?fn ?fa ?fs ?(left = true) ~n_turns ~pitch ?r2 r1 =
  let r2 = Option.value ~default:r1 r2 in
  let n_frags = Util.helical_fragments ?fn ?fa ?fs (Float.max r1 r2) in
  let r_step = (r2 -. r1) /. Float.of_int (n_turns * n_frags)
  and h_step = pitch /. Float.of_int n_frags
  and a_step = 2. *. Float.pi /. Float.of_int n_frags *. if left then -1. else 1. in
  let f i =
    let i = Float.of_int i in
    let r = r1 +. (r_step *. i)
    and a = a_step *. i in
    Float.(v3 (r *. cos a) (r *. sin a) (h_step *. i))
  in
  List.init ((n_frags * n_turns) + 1) f

let scaler ~len { x; y } =
  let step = Vec3.map (fun a -> (a -. 1.) /. Float.of_int len) (v3 x y 1.) in
  fun i -> MultMatrix.scaling @@ Vec3.map (fun a -> (a *. Float.of_int i) +. 1.) step

let twister ~len r =
  let step = r /. Float.of_int len in
  fun i -> Quaternion.(to_multmatrix @@ make (v3 0. 0. 1.) (step *. Float.of_int i))

let to_transforms ?(euler = false) ?scale ?twist path =
  let p = Array.of_list path in
  let len = Array.length p
  and id _ = MultMatrix.id in
  if len < 2 then invalid_arg "Invalid path (too few points).";
  let scale = Util.value_map_opt ~default:id (scaler ~len) scale
  and twist = Util.value_map_opt ~default:id (twister ~len) twist
  and transform =
    if euler
    then (
      let m =
        Quaternion.(to_multmatrix @@ of_euler Float.(v3 (pi /. 2.) 0. (pi /. 2.)))
      in
      fun i ->
        let { x = dx; y = dy; z = dz } =
          if i = 0
          then Vec3.(p.(1) -@ p.(0))
          else if i = len - 1
          then Vec3.(p.(i) -@ p.(i - 1))
          else Vec3.(p.(i + 1) -@ p.(i - 1))
        in
        let ay = Float.atan2 dz (Float.sqrt ((dx *. dx) +. (dy *. dy)))
        and az = Float.atan2 dy dx in
        let q = Quaternion.of_euler (v3 0. (-.ay) az) in
        MultMatrix.mul Quaternion.(to_multmatrix ~trans:p.(i) q) m )
    else (
      let accum_qs =
        let local i =
          let p1 = p.(i)
          and p2 = p.(i + 1)
          and p3 = p.(i + 2) in
          Quaternion.alignment Vec3.(normalize (p2 -@ p1)) Vec3.(normalize (p3 -@ p2))
        in
        match List.init (len - 2) local with
        | []       -> [| Quaternion.id |]
        | [ q ]    -> [| q; Quaternion.id |]
        | hd :: tl ->
          let f (acc, qs) m =
            let q = Quaternion.mul m acc in
            q, q :: qs
          in
          let _, qs = List.fold_left f (hd, [ hd; Quaternion.id ]) tl in
          Util.array_of_list_rev qs
      in
      let init =
        let cardinal =
          (* Determine an appropriate axis to pre-align the 2d shape with
                 (from normal of (0., 0., 1.)), BEFORE alignment with the initial
                 tangent of the path. Adjust for sign of major axes to prevent
                 inconsistent flipping. *)
          let similarity a b = Vec3.dot a b /. Vec3.(norm a *. norm b)
          and n = Vec3.(normalize (p.(1) -@ p.(0))) in
          let z = similarity n (v3 0. 0. 1.)
          and x = similarity n (v3 1. 0. 0.)
          and y = similarity n (v3 0. 1. 0.) in
          let abs_x = Float.abs x
          and abs_y = Float.abs y
          and abs_z = Float.abs z
          and sgn_x = Math.sign x
          and sgn_y = Math.sign y
          and sgn_z = Math.sign z in
          let comp a b =
            if Float.compare (Float.abs (a -. b)) 0.01 = 1 then Float.compare a b else 0
          in
          match comp abs_x abs_y, comp abs_x abs_z, comp abs_y abs_z with
          | 1, 1, _   -> v3 sgn_x 0. 0. (* x-axis *)
          | -1, _, 1  -> v3 0. sgn_y 0. (* y-axis *)
          | 0, -1, -1 -> v3 0. 0. sgn_z (* xy equal, but less than z *)
          | 0, _, _   -> v3 0. sgn_y 0. (* xy equal, roughly following plane *)
          | _         -> v3 0. 0. sgn_z
        in
        let d = Vec3.normalize Vec3.(p.(1) -@ p.(0)) in
        MultMatrix.mul
          Quaternion.(to_multmatrix @@ alignment cardinal d)
          Quaternion.(to_multmatrix @@ alignment (v3 0. 0. 1.) cardinal)
      in
      fun i ->
        if i = 0
        then MultMatrix.(mul (translation p.(0)) init)
        else MultMatrix.mul Quaternion.(to_multmatrix ~trans:p.(i) accum_qs.(i - 1)) init
      )
  in
  let f i = scale i |> MultMatrix.mul (twist i) |> MultMatrix.mul (transform i) in
  List.init len f

let normal = function
  | p0 :: p1 :: p2 :: poly ->
    let area_vec =
      let f (sum, last) p =
        let c = Vec3.(cross (sub last p0) (sub p last)) in
        Vec3.add c sum, p
      in
      fst @@ List.fold_left f (f (Vec3.zero, p1) p2) poly
    in
    Vec3.(normalize @@ negate area_vec)
  | _                      -> invalid_arg "Too few points to calculate polygon normal."

let coplanar ?eps t =
  try Plane.are_points_on ?eps (Plane.of_normal @@ normal t) t with
  (* too few points, or co-linear *)
  | Invalid_argument _ -> false

let to_plane = function
  | []         -> invalid_arg "Empty path cannot be converted to plane."
  | point :: t -> Plane.of_normal ~point (normal t)

let project plane = List.map (Plane.project plane)

let centroid ?(eps = Util.epsilon) = function
  | [] | [ _ ] | [ _; _ ] -> invalid_arg "Polygon must have more than two points."
  | p0 :: p1 :: tl as t   ->
    let plane = to_plane t in
    if not @@ Plane.are_points_on ~eps plane t
    then invalid_arg "Polygon must be coplanar.";
    let n = Plane.normal plane in
    let f (area_sum, p_sum, p1) p2 =
      let area = Vec3.(dot (cross (sub p2 p0) (sub p1 p0)) n) in
      area +. area_sum, Vec3.(add p_sum (add p0 (add p1 p2))), p2
    in
    let area_sum, p_sum, _ = List.fold_left f (0., Vec3.zero, p1) tl in
    if Math.approx ~eps area_sum 0.
    then invalid_arg "The polygon is self-intersecting, or its points are collinear.";
    Vec3.(sdiv p_sum (area_sum /. 3.))

let area ?(signed = false) = function
  | [] | [ _ ] | [ _; _ ] -> 0.
  | p0 :: p1 :: tl as t   ->
    let plane = to_plane t in
    if not @@ Plane.are_points_on plane t then invalid_arg "Polygon must be coplanar.";
    let n = Plane.normal plane in
    let f (area, p1) p2 = (area +. Vec3.(dot (cross (sub p1 p0) (sub p2 p0)) n)), p2 in
    let area, _ = List.fold_left f (0., p1) tl in
    if signed then area else Float.abs area

let translate p = List.map (Vec3.translate p)
let rotate r = List.map (Vec3.rotate r)
let rotate_about_pt r p = List.map (Vec3.rotate_about_pt r p)
let quaternion q = List.map (Quaternion.rotate_vec3 q)
let quaternion_about_pt q p = List.map (Quaternion.rotate_vec3_about_pt q p)
let vector_rotate ax r = quaternion (Quaternion.make ax r)
let vector_rotate_about_pt ax r = quaternion_about_pt (Quaternion.make ax r)
let multmatrix m = List.map (MultMatrix.transform m)
let scale s = List.map (Vec3.scale s)
let mirror ax = List.map (Vec3.mirror ax)
