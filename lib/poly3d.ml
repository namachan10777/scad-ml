let extrude_with_radius ?(fn = 30) ~height ~r1 ~r2 poly =
  let n1 = Float.of_int @@ Float.compare r1 0.
  and n2 = Float.of_int @@ Float.compare r2 0.
  and r1 = Float.abs r1
  and r2 = Float.abs r2
  and frac = 1. /. Float.of_int fn in
  let mid = Scad.translate (0., 0., r1) (Scad.linear_extrude ~height poly)
  and bot_f s =
    let r =
      (n1 *. Float.sqrt ((r1 *. r1) -. Float.pow (r1 -. (s *. frac *. r1)) 2.))
      -. (n1 *. r1)
    in
    Scad.offset (`Radius r) poly
    |> Scad.linear_extrude ~height:((r1 *. frac) +. 0.01)
    |> Scad.translate (0., 0., s *. frac *. r1)
  and top_f s =
    let r =
      (n2 *. Float.sqrt ((r2 *. r2) -. Float.pow (s *. frac *. r2) 2.)) -. (n2 *. r2)
    in
    Scad.offset (`Radius r) poly
    |> Scad.linear_extrude ~height:((r2 *. frac) +. 0.01)
    |> Scad.translate (0., 0., r1 +. height +. (s *. frac *. r2))
  in
  let rec loop acc i =
    if i < fn
    then (
      let s = Float.of_int i in
      loop (bot_f s :: top_f s :: acc) (i + 1) )
    else acc
  in
  Scad.union @@ loop [ mid ] 0

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
