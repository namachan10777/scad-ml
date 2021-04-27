open Scad_ml

let square = Model.square ~center:true (10., 10.)
let circle = Model.circle 10.
let triangle_polygon = Model.polygon [ -0.5, 0.; 0., 1.; 0.5, 0. ]
let linear_extrude_circle = Model.linear_extrude ~height:10. circle

let rotate_extrude_triangle =
  Model.rotate_extrude (Model.translate (3., 0., 0.) triangle_polygon)

let hello = Model.text "Hello, world!"

let vertical_text =
  Model.text
    "Tall Text"
    ~spacing:5.
    ~valign:Core.Text.Top
    ~direction:Core.Text.TopToBottom

let () =
  print_endline "Building test scads...";
  Util.write (open_out "square.scad") square;
  Util.write (open_out "circle.scad") circle;
  Util.write (open_out "triangle_polygon.scad") triangle_polygon;
  Util.write (open_out "linear_extrude_circle.scad") linear_extrude_circle;
  Util.write (open_out "rotate_extrude_triangle.scad") rotate_extrude_triangle;
  Util.write (open_out "hello.scad") hello;
  Util.write (open_out "vertical_text.scad") vertical_text;
  print_endline "Done!"