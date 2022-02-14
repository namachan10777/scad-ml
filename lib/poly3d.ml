open Util

type t =
  { n_points : int
  ; points : Vec3.t list
  ; faces : int list list
  }

type caps =
  [ `Capped
  | `Looped
  | `Open
  | `Caps of int list * int list
  ]

let empty = { n_points = 0; points = []; faces = [] }
let n_points t = t.n_points
let points t = t.points
let faces t = t.faces
let make ~points ~faces = { n_points = List.length points; points; faces }

let of_layers ?(caps = `Capped) layers =
  let looped =
    match caps with
    | `Looped -> true
    | _       -> false
  in
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
      let last_seg = n_layers - (if looped then 0 else 1) - 1
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
      let faces = loop [] 0 0
      and top_offset = n_facets * (n_layers - 1) in
      match caps with
      | `Capped                ->
        let bottom_cap = List.init n_facets (fun i -> n_facets - 1 - i)
        and top_cap = List.init n_facets (fun i -> i + top_offset) in
        top_cap :: bottom_cap :: faces
      | `Open | `Looped        -> faces
      | `Caps ([], top_cap)    -> List.map (( + ) top_offset) top_cap :: faces
      | `Caps (bottom_cap, []) -> bottom_cap :: faces
      | `Caps (bot, top)       -> List.map (( + ) top_offset) top :: bot :: faces
    in
    { n_points = n_layers * n_facets; points; faces = List.rev faces }

let tri_mesh ?(looped = false) rows =
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
    let _, faces = List.fold_left f (hd, []) (if looped then tl @ [ hd ] else tl)
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

let sweep ?(winding = `CCW) ?caps ~transforms shape =
  let shape = enforce_winding winding shape in
  List.map
    (fun m -> List.map (fun (x, y) -> MultMatrix.transform m (x, y, 0.)) shape)
    transforms
  |> of_layers ?caps

let linear_extrude ?winding ?slices ?fa ?scale ?twist ?(center = false) ~height shape =
  let slices = helical_slices ?fa ?fn:slices (Option.value ~default:0. twist) in
  let bot = if center then height /. -2. else 0.
  and s = height /. Float.of_int slices in
  let transforms =
    List.init (slices + 1) (fun i -> 0., 0., (Float.of_int i *. s) +. bot)
    |> Path3d.to_transforms ?scale ?twist
  in
  sweep ?winding ~transforms shape

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

let cartesian_plot ~min_x ~x_steps ~max_x ~min_y ~y_steps ~max_y plot =
  let x_step = (max_x -. min_x) /. Float.of_int x_steps
  and y_step = (max_y -. min_y) /. Float.of_int y_steps in
  let max_x = max_x +. (0.001 *. x_step)
  and max_y = max_y +. (0.001 *. y_step)
  (* Stay above plane to guarantee manifold. Bottom always at 0. *)
  and min_plot = 0.0005 *. (x_step +. y_step) in
  let xs_rev =
    List.init (x_steps + 1) (fun i -> (Float.of_int (x_steps - i) *. x_step) +. min_x)
  in
  let edge y =
    (max_x, y, 0.)
    :: (min_x, y, 0.)
    :: List.fold_left (fun acc x -> (x, y, 0.0001) :: acc) [] xs_rev
  in
  let layers =
    let outer i layers =
      let y = (Float.of_int i *. y_step) +. min_y in
      let layer l x =
        let z = Float.max min_plot (plot ~x ~y) in
        (x, y, z) :: l
      in
      ((max_x, y, 0.) :: (min_x, y, 0.) :: List.fold_left layer [] xs_rev) :: layers
    in
    Util.fold_init (y_steps + 1) outer [ edge (min_y -. (0.001 *. y_step)) ]
  in
  of_layers @@ (edge (max_y +. (0.001 *. y_step)) :: layers)

let polar_plot ?r_step ~max_r plot =
  let r_step, a_steps =
    match r_step with
    | Some s -> s, Float.(to_int (ceil (max_r *. 2. *. pi /. s /. 8.))) * 8
    | None   -> 2. *. Float.pi *. max_r /. 360., 360
  in
  let r_steps = Float.(to_int @@ ceil (max_r /. r_step)) in
  let r_step = (max_r -. (1e-6 *. r_step)) /. Float.of_int r_steps in
  let min_plot = 0.001 *. r_step
  and angles_rev =
    let step = 2. *. Float.pi /. Float.of_int a_steps in
    Util.fold_init (a_steps + 1) (fun i acc -> (Float.of_int i *. step) :: acc) []
  in
  let bot =
    let f ps a = Float.(max_r *. cos a, max_r *. sin a, 0.) :: ps in
    List.fold_left f [] angles_rev
  in
  let f i layers =
    let r = max_r -. (Float.of_int i *. r_step) in
    let layer l a =
      let z = Float.max min_plot (plot ~r ~a) in
      Float.(r *. cos a, r *. sin a, z) :: l
    in
    List.fold_left layer [] angles_rev :: layers
  in
  of_layers @@ Util.fold_init (r_steps + 1) f [ bot ]

let axial_plot ?(fn = 60) ~min_z ~z_steps ~max_z plot =
  let z_step = (max_z -. min_z) /. Float.of_int z_steps in
  let min_plot = 0.001 *. z_step
  and angles_rev =
    let step = 2. *. Float.pi /. Float.of_int fn in
    Util.fold_init (fn + 1) (fun i acc -> (Float.of_int i *. step) :: acc) []
  in
  let f i layers =
    let z = min_z +. (Float.of_int i *. z_step) in
    let layer l a =
      let r = Float.max min_plot (plot ~z ~a) in
      Float.(r *. cos a, r *. sin a, z) :: l
    in
    List.fold_left layer [] angles_rev :: layers
  in
  of_layers @@ Util.fold_init (z_steps + 1) f []

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
