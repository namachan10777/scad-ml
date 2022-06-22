exception FailedExport of string * string

let openscad = if Sys.unix then "openscad" else "openscad.com"
let sz = 8192
let bytes = Bytes.create sz

let file_to_string n =
  let fd = Unix.openfile n [ O_RDONLY ] 0o777 in
  let b = Buffer.create sz in
  let rec loop () =
    match Unix.read fd bytes 0 sz with
    | 0 -> ()
    | r when r = sz ->
      Buffer.add_bytes b bytes;
      loop ()
    | r ->
      Buffer.add_bytes b (Bytes.sub bytes 0 r);
      loop ()
  in
  loop ();
  Unix.close fd;
  Buffer.contents b

let script out_path scad_path =
  let format =
    let ext = Filename.extension out_path in
    String.sub ext 1 (String.length ext - 1)
  and err_name = Filename.temp_file "scad_ml_" "_err" in
  let err = Unix.openfile err_name [ O_WRONLY; O_CREAT; O_TRUNC ] 0o777 in
  let pid =
    Unix.create_process
      openscad
      [| openscad
       ; "-q"
       ; "-o"
       ; out_path
       ; "--export-format"
       ; (if String.equal format "stl" then "binstl" else format)
       ; scad_path
      |]
      Unix.stdin
      Unix.stdout
      err
  in
  ignore @@ Unix.waitpid [] pid;
  Unix.close err;
  let e = file_to_string err_name in
  Sys.remove err_name;
  if String.length e > 0 then raise (FailedExport (out_path, e))
