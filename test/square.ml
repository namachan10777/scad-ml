open Scad_ml

let square = Scad.square ~center:true (10., 10.)
let () = print_string (Scad.to_string square)
