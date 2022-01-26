open Util

type layer = Vec3.t list

type t =
  { n_layers : int
  ; n_facets : int
  ; points : Vec3.t list
  ; faces : int list list
  }

(* let transform_layer m = List.map (MultMatrix.transform m) *)

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
  { n_layers; n_facets; points; faces }

let sweep ?closed ~transforms shape =
  let shape =
    if Float.equal (Poly2d.clockwise_sign shape) 1. then List.rev shape else shape
  in
  List.map
    (fun m -> List.map (fun (x, y) -> MultMatrix.transform m (x, y, 0.)) shape)
    transforms
  |> of_layers ?closed

let linear_extrude ?slices ?scale ?(twist = 0.) ?(center = false) ~height shape =
  let slices = helical_slices ?fn:slices twist in
  let bot = if center then height /. -2. else 0.
  and s = height /. Float.of_int slices
  and twist = if Float.abs twist > 0. then Some twist else None in
  let transforms =
    List.init (slices + 1) (fun i -> 0., 0., (Float.of_int i *. s) +. bot)
    |> Path.to_transforms ?scale ?twist
  in
  sweep ~transforms shape

let to_scad ?convexity { points; faces; _ } = Scad.polyhedron ?convexity points faces
