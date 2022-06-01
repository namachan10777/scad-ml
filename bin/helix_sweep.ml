open Scad_ml

let  () =
  let scad =
    let path = Path3.helix ~left:true ~pitch:30. ~n_turns:10 ~r2:100. 50. in
    let transforms =
      Path3.to_transforms ~twist:(-240. /. 180. *. Float.pi) ~euler:false path
    in
    Mesh.(to_scad @@ sweep ~transforms elbow_shape)
  and oc = open_out "helix_sweep.scad" in
  Scad.write oc scad;
  close_out oc
