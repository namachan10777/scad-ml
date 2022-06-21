(* Note that progress indicators (names being exported) will not appear as they
   occur unless --no-buffer is passed to dune when building @export_examples.
   (e.g. dune build @export_examples --no-buffer)

   This does however disable parallelism, so one should only do so if openscad
   is failing to make it more clear which file is causing trouble. *)
let () =
  print_endline "\nExporting example stls...";
  let openscad = if Sys.unix then "openscad" else "openscad.com" in
  for i = 1 to Array.length Sys.argv - 1 do
    let n = Filename.(chop_extension @@ basename Sys.argv.(i)) in
    Printf.printf "=> %s\n%!" n;
    ignore
    @@ Sys.command
    @@ (Printf.sprintf "%s -q -o \"%s.stl\" --export-format binstl \"%s\"")
         openscad
         n
         Sys.argv.(i);
    flush_all ()
  done
