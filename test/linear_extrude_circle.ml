open Scad_ml

let circle = Scad.circle 10.
let linear_extrude_circle = Scad.linear_extrude ~height:10. circle
let () = print_string (Scad.to_string linear_extrude_circle)
