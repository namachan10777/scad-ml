open Scad_ml

let () =
  let poly =
    [ -10., -1.; -10., 6.; -7., 6.; -7., 1.; 7., 1.; 7., 6.; 10., 6.; 10., -1. ]
    |> Path2.of_tups
    |> Poly2.make
  in
  let scad =
    let path = Path3.helix ~left:true ~pitch:30. ~n_turns:10 ~r2:100. 50. in
    Mesh.(to_scad @@ path_extrude ~twist:(-240. /. 180. *. Float.pi) ~path poly)
  in
  Scad.to_file "helix_sweep.scad" scad
