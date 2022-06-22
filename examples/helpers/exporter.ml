open Scad_ml

let () =
  let f i =
    let n = Filename.(chop_extension @@ basename Sys.argv.(i + 1)) ^ ".stl" in
    try Export.script n Sys.argv.(i + 1) with
    | Export.FailedExport (n, e) -> Printf.printf "Failed to export %s:\n%s\n%!" n e
  in
  List.init (Array.length Sys.argv - 1) (Thread.create f) |> List.iter Thread.join
