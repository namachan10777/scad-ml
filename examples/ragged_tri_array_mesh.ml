open Scad_ml

let  () =
  let scad =
    List.init 10 (fun y ->
        let y = y + 1 in
        List.init y (fun x -> Float.(v3 (of_int x) (of_int y) (of_int y))) )
    |> Mesh.of_ragged
    |> Mesh.to_scad
  and oc = open_out "ragged_tri_array_mesh.scad" in
  Scad.write oc scad;
  close_out oc
