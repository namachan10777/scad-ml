open Scad_ml

let triangle_polygon = Scad.polygon [ -0.5, 0.; 0., 1.; 0.5, 0. ]

let rotate_extrude_triangle =
  Scad.rotate_extrude (Scad.translate (3., 0., 0.) triangle_polygon)

let () = print_string (Scad.to_string rotate_extrude_triangle)
