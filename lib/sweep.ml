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

let sweep ?(closed = false) ?convexity ~transforms shape =
  let len = List.length transforms
  and facets = List.length shape
  and pts =
    List.concat_map
      (fun m -> List.map (fun p -> MultMatrix.transform m p) shape)
      transforms
  in
  let faces =
    let last_seg = len - (if closed then 0 else 1) - 1
    and last_face = facets - 1 in
    let rec loop acc seg face =
      let acc =
        let s0 = seg mod len
        and s1 = (seg + 1) mod len
        and f1 = (face + 1) mod facets in
        [ (s0 * facets) + face
        ; (s0 * facets) + f1
        ; (s1 * facets) + f1
        ; (s1 * facets) + face
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
      let bottom_cap = List.init facets (fun i -> facets - 1 - i)
      and top_cap = List.init facets (fun i -> i + (facets * (len - 1))) in
      List.rev @@ (top_cap :: bottom_cap :: looped_faces) )
  in
  Scad.polyhedron ?convexity pts faces
