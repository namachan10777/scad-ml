open Scad_ml

let () =
  let scad =
    Mesh.helix_extrude
      ~scale:(v2 1. 1.)
      ~left:true
      ~pitch:30.
      ~n_turns:10
      ~r2:100.
      50.
      elbow_shape
    |> Mesh.to_scad
  and oc = open_out "helix_extrude.scad" in
  Scad.write oc scad;
  close_out oc
