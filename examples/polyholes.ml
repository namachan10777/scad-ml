(** {0 Polygons with holes} *)
open Scad_ml

let () =
  let scad =
    let shape =
      let holes =
        let s =
          Path2.square ~center:true (v2 2. 2.)
          |> Path2.rotate (Float.pi /. 4.)
          |> List.rev
        in
        Path2.[ s; translate (v2 (-2.) (-2.)) s; translate (v2 2. 2.) s ]
      in
      Poly2.make ~holes (Path2.square ~center:true (v2 10. 10.))
    in
    let poly =
      Mesh.linear_extrude ~height:1. shape
      |> Mesh.to_scad
      |> Scad.color ~alpha:0.5 Color.Silver
    and reference =
      Poly2.to_scad shape
      |> Scad.linear_extrude ~height:1.
      |> Scad.translate (v3 0. 0. (-3.))
      |> Scad.color ~alpha:0.5 Color.BlueViolet
    in
    Scad.union [ poly; reference ]
  in
  Scad.to_file "polyholes.scad" scad
