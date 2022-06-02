open Scad_ml

let  () =
  let scad =
    let pts = Path3.helix ~left:false ~pitch:5. ~n_turns:10 ~r2:10. 5. in
    let s = Scad.color Color.Red @@ Scad.sphere 1. in
    Scad.union @@ List.map (fun p -> Scad.translate p s) pts
  and oc = open_out "helix_path_points.scad" in
  Scad.write oc scad;
  close_out oc
