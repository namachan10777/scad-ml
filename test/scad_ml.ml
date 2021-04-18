open Scad_ml

let square = Core.Square { size = 10., 10.; center = true }
let circle = Core.Circle { r = 10.; fa = None; fs = None; fn = None }

let triangle_polygon =
  Core.Polygon { points = [ -0.5, 0.; 0., 1.; 0.5, 0. ]; paths = None; convexity = 2 }

let linear_extrude_circle = Model.linear_extrude ~height:10. circle

let rotate_extrude_triangle =
  Core.RotateExtrude
    { src = Model.translate (3., 0., 0.) triangle_polygon
    ; angle = Some (Core.pi *. 2.)
    ; convexity = 2
    ; fa = None
    ; fs = None
    ; fn = None
    }

let () =
  print_endline "Building test scads...";
  Util.write (open_out "square.scad") square;
  Util.write (open_out "circle.scad") circle;
  Util.write (open_out "triangle_polygon.scad") triangle_polygon;
  Util.write (open_out "linear_extrude_circle.scad") linear_extrude_circle;
  Util.write (open_out "rotate_extrude_triangle.scad") rotate_extrude_triangle;
  print_endline "Done!"
