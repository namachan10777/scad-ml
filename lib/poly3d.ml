let transforms_of_path path =
  let p = Array.of_list path in
  let len = Array.length p in
  let f i =
    let open Vec3 in
    let tangent =
      ( if i = 0
      then p.(1) <-> p.(0)
      else if i = len - 1
      then p.(i) <-> p.(i - 1)
      else p.(i + 1) <-> p.(i - 1) )
      |> normalize
    in
    Quaternion.(to_multmatrix ~trans:p.(i) @@ alignment (0., 0., 1.) tangent)
  in
  List.init len f

let stitch_polyhedron ?(closed = false) ?convexity layers =
  let n_layers = List.length layers
  and n_facets = List.length (List.hd layers)
  and pts = List.concat layers in
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
  Scad.polyhedron ?convexity pts faces

let sweep ?closed ?convexity ~transforms shape =
  List.map
    (fun m -> List.map (fun (x, y) -> MultMatrix.transform m (x, y, 0.)) shape)
    transforms
  |> stitch_polyhedron ?closed ?convexity
