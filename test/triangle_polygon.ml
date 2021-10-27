open Scad_ml

let triangle_polygon = Scad.polygon [ -0.5, 0.; 0., 1.; 0.5, 0. ]
let () = print_string (Scad.to_string triangle_polygon)
