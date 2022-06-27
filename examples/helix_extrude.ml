(** {0 Helical Extrusion} *)
open Scad_ml

let poly =
  [ -10., -1.; -10., 6.; -7., 6.; -7., 1.; 7., 1.; 7., 6.; 10., 6.; 10., -1. ]
  |> Path2.of_tups
  |> Poly2.make

let mesh =
  Mesh.helix_extrude ~scale:(v2 1. 1.) ~left:true ~pitch:30. ~n_turns:10 ~r2:100. 50. poly

let () = Scad.to_file "helix_extrude.scad" (Mesh.to_scad mesh)
