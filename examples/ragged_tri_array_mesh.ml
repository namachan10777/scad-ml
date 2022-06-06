open Scad_ml

let () =
  let scad =
    List.init 10 (fun y ->
        let y = y + 1 in
        List.init y (fun x -> Float.(v3 (of_int x) (of_int y) (of_int y))) )
    |> Mesh.of_ragged
    |> Mesh.to_scad
  in
  Scad.to_file "ragged_tri_array_mesh.scad" scad
