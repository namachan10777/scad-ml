open Scad_ml

let () =
  let f i =
    let n = Filename.(chop_extension @@ basename Sys.argv.(i + 1)) ^ ".png" in
    let f e = Printf.printf "Failed to take snapshot %s:\n%s\n%!" n e in
    Result.iter_error f Export.(snapshot ~colorscheme:TomorrowNight n Sys.argv.(i + 1))
  in
  List.init (Array.length Sys.argv - 1) (Thread.create f) |> List.iter Thread.join
