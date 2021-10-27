open Scad_ml

let hello = Scad.text "Hello, world!"
let () = print_string (Scad.to_string hello)
