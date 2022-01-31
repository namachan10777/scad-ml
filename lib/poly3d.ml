open Util

type t =
  { n_points : int
  ; points : Vec3.t list
  ; faces : int list list
  }

let empty = { n_points = 0; points = []; faces = [] }
let n_points t = t.n_points
let points t = t.points
let faces t = t.faces

let of_layers ?(closed = false) layers =
  let n_layers = List.length layers
  and n_facets = List.length (List.hd layers)
  and points = List.concat layers in
  let faces =
    let last_seg = n_layers - (if closed then 0 else 1) - 1
    and last_face = n_facets - 1 in
    let rec loop acc seg face =
      let acc =
        let s0 = seg mod n_layers
        and s1 = (seg + 1) mod n_layers
        and f1 = (face + 1) mod n_facets in
        [ (s0 * n_facets) + face
        ; (s0 * n_facets) + f1
        ; (s1 * n_facets) + f1
        ; (s1 * n_facets) + face
        ]
        :: acc
      in
      if face = last_face
      then if seg = last_seg then acc else loop acc (seg + 1) 0
      else loop acc seg (face + 1)
    in
    let looped_faces = loop [] 0 0 in
    if closed
    then List.rev looped_faces
    else (
      let bottom_cap = List.init n_facets (fun i -> n_facets - 1 - i)
      and top_cap = List.init n_facets (fun i -> i + (n_facets * (n_layers - 1))) in
      List.rev @@ (top_cap :: bottom_cap :: looped_faces) )
  in
  { n_points = n_layers * n_facets; points; faces }

let enforce_winding w shape =
  let reverse =
    match w with
    | `CCW     -> Float.equal (Poly2d.clockwise_sign shape) 1.
    | `CW      -> Float.equal (Poly2d.clockwise_sign shape) (-1.)
    | `NoCheck -> false
  in
  if reverse then List.rev shape else shape

let sweep ?(winding = `CCW) ?closed ~transforms shape =
  let shape = enforce_winding winding shape in
  List.map
    (fun m -> List.map (fun (x, y) -> MultMatrix.transform m (x, y, 0.)) shape)
    transforms
  |> of_layers ?closed

let linear_extrude ?slices ?scale ?twist ?(center = false) ~height shape =
  let slices = helical_slices ?fn:slices (Option.value ~default:0. twist) in
  let bot = if center then height /. -2. else 0.
  and s = height /. Float.of_int slices in
  let transforms =
    List.init (slices + 1) (fun i -> 0., 0., (Float.of_int i *. s) +. bot)
    |> Path3d.to_transforms ?scale ?twist
  in
  sweep ~transforms shape

let helix_extrude ?fn ?fa ?fs ?scale ?twist ?(left = true) ~n_turns ~pitch ?r2 r1 shape =
  let r2 = Option.value ~default:r1 r2 in
  let n_frags = helical_fragments ?fn ?fa ?fs (Float.max r1 r2) in
  let rot_sign, winding = if left then -1., `CCW else 1., `CW in
  let a_step = 2. *. Float.pi /. Float.of_int n_frags *. rot_sign
  and ax =
    let a = Float.(atan2 (pitch /. of_int n_frags) (pi *. 2. *. r1 /. of_int n_frags)) in
    (a *. rot_sign) +. (Float.pi /. 2.)
  in
  let transforms =
    let path = Path3d.helix ?fn ?fa ?fs ~left ~n_turns ~pitch ~r2 r1 in
    let len = List.length path
    and id _ = MultMatrix.id in
    let scale = Util.value_map_opt ~default:id (Path3d.scaler ~len) scale
    and twist = Util.value_map_opt ~default:id (Path3d.twister ~len) twist in
    let f i trans =
      scale i
      |> MultMatrix.mul (twist i)
      |> MultMatrix.mul
           Quaternion.(to_multmatrix ~trans (of_euler (ax, 0., a_step *. Float.of_int i)))
    in
    List.mapi f path
  in
  sweep ~winding ~transforms shape

let join = function
  | [] -> empty
  | [ t ] -> t
  | { n_points; points; faces } :: ts ->
    let f (n, ps, fs) t =
      let offset = List.map (List.map (( + ) n)) t.faces in
      n + t.n_points, t.points :: ps, offset :: fs
    in
    let n_points, ps, fs = List.fold_left f (n_points, [ points ], [ faces ]) ts in
    { n_points; points = List.concat (List.rev ps); faces = List.concat (List.rev fs) }

let rev_faces t = { t with faces = List.map List.rev t.faces }
let translate p t = { t with points = Path3d.translate p t.points }
let rotate r t = { t with points = Path3d.rotate r t.points }
let rotate_about_pt r p t = { t with points = Path3d.rotate_about_pt r p t.points }
let quaternion q t = { t with points = Path3d.quaternion q t.points }

let quaternion_about_pt q p t =
  { t with points = Path3d.quaternion_about_pt q p t.points }

let vector_rotate ax r = quaternion (Quaternion.make ax r)
let vector_rotate_about_pt ax r = quaternion_about_pt (Quaternion.make ax r)
let multmatrix m t = { t with points = Path3d.multmatrix m t.points }
let scale s t = { t with points = Path3d.scale s t.points }
let mirror ax t = rev_faces { t with points = Path3d.mirror ax t.points }
let to_scad ?convexity { points; faces; _ } = Scad.polyhedron ?convexity points faces
