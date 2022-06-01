open Scad_ml

let () =
  let step = 0.005 in
  let path =
    let f i =
      let t = Float.of_int i *. step in
      let x =
        ((t /. 1.5) +. 0.5) *. 100. *. Float.cos (6. *. 360. *. t *. Float.pi /. 180.)
      and y =
        ((t /. 1.5) +. 0.5) *. 100. *. Float.sin (6. *. 360. *. t *. Float.pi /. 180.)
      and z = 200. *. (1. -. t) in
      v3 x y z
    in
    List.init (Int.of_float (1. /. step)) f
  in
  let () =
    let transforms = Path3.to_transforms path
    and oc = open_out "sweep_path.scad" in
    (Scad.write oc @@ Mesh.(to_scad @@ sweep ~transforms Elbow.poly));
    close_out oc
  in
  let transforms = Path3.to_transforms ~euler:true path
  and oc = open_out "sweep_path_euler.scad" in
  (Scad.write oc @@ Mesh.(to_scad @@ sweep ~transforms Elbow.poly));
  close_out oc
