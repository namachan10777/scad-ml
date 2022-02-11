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
let make ~points ~faces = { n_points = List.length points; points; faces }

(* TODO:
  - add some more control over the sealing of the shapes. Instead of only
    capping or looping, also allow for there to be no caps added (non-manifold)
   - also, "closed" is a bit overloaded. Maybe "capped", "seal"/"sealed"/"seals".
   - variant type? `Looped | `Capped | `Custom of [`Auto | `Manual of int list]
    tuple
   - would allow for all of the variations. Is there a cleaner way?
   - Being able to leave off the caps should allow for building polyhedrons
    with holes by joining each of the shapes along with the special caps*)
let of_layers ?(closed = false) layers =
  match layers with
  | []       -> empty
  | [ _ ]    -> raise (Invalid_argument "Only one layer provided.")
  | hd :: tl ->
    let n_layers = List.length layers
    and n_facets = List.length hd
    and points = List.concat layers in
    if not (List.for_all (fun l -> List.length l = n_facets) tl)
    then raise (Invalid_argument "Inconsistent layer length.");
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

let tri_mesh ?(closed = false) rows =
  let starts_lenghts, points =
    let f (start, starts_lengths, points) row =
      let g (i, ps) p = i + 1, p :: ps in
      let len, points = List.fold_left g (0, points) row in
      start + len, (start, len) :: starts_lengths, points
    in
    let _, starts_lengths, points = List.fold_left f (0, [], []) rows in
    List.rev starts_lengths, List.rev points
  in
  match starts_lenghts with
  | [] | [ _ ] -> empty
  | hd :: tl   ->
    let f ((start, len), faces) ((next_start, next_len) as next) =
      let faces =
        match next_len - len with
        | 0     ->
          let a i = [ i + start; i + start + 1; i + next_start ]
          and b i = [ i + start + 1; i + next_start + 1; i + next_start ] in
          Util.prepend_init (len - 1) a faces |> Util.prepend_init (len - 1) b
        | 1     ->
          let a i = [ i + start; i + start + 1; i + next_start + 1 ]
          and b i = [ i + start; i + next_start + 1; i + next_start ] in
          Util.prepend_init (len - 1) a faces |> Util.prepend_init len b
        | -1    ->
          let a i = [ i + start + 1; i + next_start + 1; i + next_start ]
          and b i = [ i + start; i + start + 1; i + next_start ] in
          Util.prepend_init (len - 2) a faces |> Util.prepend_init (len - 1) b
        | 2     ->
          let count = Float.(to_int @@ floor @@ ((of_int len -. 1.) /. 2.)) in
          let a i = [ i + start; i + start + 1; i + next_start + 1 ]
          and b i =
            let i = i + count in
            [ i + start; i + start + 1; i + next_start + 2 ]
          and c i = [ i + start; i + next_start + 1; i + next_start ]
          and d i =
            let i = i + count + 1 in
            [ i + start - 1; i + next_start + 1; i + next_start ]
          in
          Util.prepend_init count a faces
          |> Util.prepend_init (len - count - 1) b
          |> Util.prepend_init (count + 1) c
          |> Util.prepend_init (next_len - 2 - count) d
        | -2    ->
          let count = Float.(to_int @@ floor @@ ((of_int len -. 1.) /. 2.)) in
          let a i = [ i + next_start; i + start + 1; i + next_start + 1 ]
          and b i =
            let i = i + count - 1 in
            [ i + next_start; i + start + 2; i + next_start + 1 ]
          and c i = [ i + next_start; i + start; i + start + 1 ]
          and d i =
            let i = i + count in
            [ i + next_start - 1; i + start; i + start + 1 ]
          in
          Util.prepend_init (count - 1) a faces
          |> Util.prepend_init (len - count - 2) b
          |> Util.prepend_init count c
          |> Util.prepend_init (next_len + 1 - count) d
        | delta ->
          let s = Printf.sprintf "Unsupported layer length difference of %i" delta in
          raise (Invalid_argument s)
      in
      next, faces
    in
    let _, faces = List.fold_left f (hd, []) (if closed then tl @ [ hd ] else tl)
    and verts = Array.of_list points in
    let cull_degenerate =
      let v = Array.unsafe_get verts in
      let not_degen a b = Float.compare (Vec3.distance (v a) (v b)) Util.epsilon = 1 in
      function
      | [ i0; i1; i2 ] -> not_degen i0 i1 && not_degen i1 i2 && not_degen i2 i0
      | _              -> failwith "unreachable"
    in
    { n_points = Array.length verts; points; faces = List.filter cull_degenerate faces }

let mesh_of_layer ?(reverse = false) layer =
  let n_points, points, face =
    List.fold_left (fun (n, ps, fs) p -> n + 1, p :: ps, n :: fs) (0, [], []) layer
  in
  { n_points; points; faces = [ (if reverse then List.rev face else face) ] }

let enforce_winding w shape =
  let reverse =
    match w with
    | `CCW     -> Float.equal (Path2d.clockwise_sign shape) 1.
    | `CW      -> Float.equal (Path2d.clockwise_sign shape) (-1.)
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

let cartesian_plot ~min_x ~step_x ~max_x ~min_y ~step_y ~max_y plot =
  let max_x = max_x +. (0.001 *. step_x)
  and max_y = max_y +. (0.001 *. step_y)
  (* stay above plane to guarantee manifold. *)
  and min_plot = 0.0005 *. (step_x +. step_y)
  and x_steps = Float.(to_int @@ ((max_x -. min_x) /. step_x)) + 1
  and y_steps = Float.(to_int @@ ((max_y -. min_y) /. step_y)) + 1 in
  let min_y_edge =
    let y = min_y -. (0.001 *. step_y) in
    let f i acc =
      let x = (Float.of_int i *. step_x) +. min_x in
      (x, y, 0.0001) :: acc
    and init = [ min_x, y, 0.; max_x, y, 0. ] in
    List.rev @@ Util.fold_init x_steps f init
  in
  let body =
    let outer i acc_out =
      let y = (Float.of_int i *. step_y) +. min_y in
      let inner j acc_in =
        let x = (Float.of_int j *. step_x) +. min_x in
        let z = plot x y in
        (x, y, if z < min_plot then min_plot else z) :: acc_in
      and init = [ min_x, y, 0.; max_x, y, 0. ] in
      List.rev (Util.fold_init x_steps inner init) :: acc_out
    in
    Util.fold_init y_steps outer [ min_y_edge ]
  in
  let max_y_edge =
    let y = max_y +. (0.001 *. step_y) in
    let f i acc =
      let x = (Float.of_int i *. step_x) +. min_x in
      (x, y, 0.0001) :: acc
    and init = [ min_x, y, 0.; max_x, y, 0. ] in
    List.rev @@ Util.fold_init x_steps f init
  in
  of_layers @@ List.rev (max_y_edge :: body)

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

let add_face face t = { t with faces = face :: t.faces }
let add_faces faces t = { t with faces = List.rev_append faces t.faces }
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
