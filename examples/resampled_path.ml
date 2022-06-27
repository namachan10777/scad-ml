(** {0 Path resampling} *)
open Scad_ml

let () =
  let path = [ v3 0. 0. 0.; v3 5. 5. 5.; v3 5. 5. 15. ] in
  let resampled = Path3.resample ~freq:(`Spacing 1.) path in
  let old_marks =
    let s = Scad.color Color.Red @@ Scad.sphere 0.5 in
    List.map (fun p -> Scad.translate p s) path
  and new_marks =
    let s = Scad.color ~alpha:0.1 Color.Yellow @@ Scad.sphere 1.0 in
    List.map (fun p -> Scad.translate p s) resampled
  in
  let scad = Scad.union (old_marks @ new_marks) in
  Scad.to_file "resampled_path.scad" scad
