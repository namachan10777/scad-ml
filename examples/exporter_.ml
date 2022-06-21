let openscad = if Sys.unix then "openscad" else "openscad.com"
let sz = 8192
let bytes = Bytes.create sz

let file_to_string n =
  let fd = Unix.openfile n [ O_RDONLY ] 0o777 in
  let b = Buffer.create sz in
  let rec loop () =
    match Unix.read fd bytes 0 sz with
    | 0 -> ()
    | r ->
      Buffer.add_bytes b (Bytes.sub bytes 0 r);
      loop ()
  in
  loop ();
  Unix.close fd;
  String.of_bytes (Buffer.to_bytes b)

let () =
  let run n scad =
    let err_name = Printf.sprintf "%s_err" n in
    let err = Unix.openfile err_name [ O_WRONLY; O_CREAT; O_TRUNC ] 0o777 in
    let pid =
      Unix.create_process
        openscad
        [| openscad
         ; "-q"
         ; "-o"
         ; Printf.sprintf "%s.stl" n
         ; "--export-format"
         ; "binstl"
         ; Printf.sprintf "%s" scad
        |]
        Unix.stdin
        Unix.stdout
        err
    in
    ignore @@ Unix.waitpid [] pid;
    Unix.close err;
    ( match file_to_string err_name with
    | "" -> ()
    | e  -> Printf.printf "Failed to export %s:\n%s\n%!" n e );
    Sys.remove err_name
  in
  let f i =
    let n = Filename.(chop_extension @@ basename Sys.argv.(i + 1)) in
    run n Sys.argv.(i + 1)
  in
  List.init (Array.length Sys.argv - 1) (Thread.create f) |> List.iter Thread.join
