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

let close_points ?convexity ?bot_pt ?top_pt layers =
  let n = List.length layers in
  let pts =
    let a = Array.make n [||] in
    let rec fill i = function
      | []       -> a
      | hd :: tl ->
        Array.unsafe_set a i (Array.of_list hd);
        fill (i + 1) tl
    in
    fill 0 layers
  in
  let p = Array.length pts.(0) in
  for i = 0 to n - 1 do
    let l = Array.length (Array.unsafe_get pts i) in
    if p <> l
    then (
      let msg =
        Printf.sprintf "Inconsistent layer length: layer %i -> %i (first = %i)." i l p
      in
      failwith msg )
  done;
  let size = n * p in
  let bot_face, loop_offset =
    match bot_pt with
    | Some _ ->
      let f i = [ 0; i + 1; i + ((i + 1) mod p) ] in
      List.init p f, 1
    | None   -> [ List.init p (fun i -> i) ], 0
  in
  let top_face =
    let top_offset = loop_offset + size - p in
    match top_pt with
    | Some _ ->
      let seal_offset = top_offset + p in
      let f i = [ seal_offset; top_offset + ((i + 1) mod p); top_offset + 1 ] in
      List.init p f
    | None   ->
      let last = loop_offset + size - 1 in
      [ List.init p (fun i -> last - i) ]
  and body_faces =
    let rec loop acc i j k =
      let acc =
        ( if k = 0
        then
          [ loop_offset + (i * p) + j
          ; loop_offset + ((i + 1) * p) + j
          ; loop_offset + ((i + 1) * p) + ((j + 1) mod p)
          ]
        else
          [ loop_offset + (i * p) + j
          ; loop_offset + ((i + 1) * p) + ((j + 1) mod p)
          ; loop_offset + (i * p) + ((j + 1) mod p)
          ] )
        :: acc
      in
      if k < 1
      then loop acc i j (k + 1)
      else if j < p - 1
      then loop acc i (j + 1) 0
      else if i < n - 2
      then loop acc (i + 1) 0 0
      else acc
    in
    loop [] 0 0 0
  in
  let points =
    let get i = Array.(unsafe_get (unsafe_get pts (i / p)) (i mod p)) in
    let f, last =
      match top_pt with
      | Some pt -> (fun i -> if i < size then get i else pt), size + 1
      | None    -> get, size
    in
    match bot_pt with
    | Some b -> b :: List.init last f
    | None   -> List.init last f
  in
  Scad.polyhedron ?convexity points (List.concat [ bot_face; body_faces; top_face ])
