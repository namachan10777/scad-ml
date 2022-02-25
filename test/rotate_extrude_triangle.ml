open Scad_ml

let triangle_polygon = Scad.polygon [ v2 (-0.5) 0.; v2 0. 1.; v2 0.5 0. ]

let rotate_extrude_triangle =
  Scad.rotate_extrude (Scad.translate (v3 3. 0. 0.) triangle_polygon)

let () = print_string (Scad.to_string rotate_extrude_triangle)
