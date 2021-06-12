open Scad_ml

let mat =
  let a = Math.pi /. 4. in
  let cos_a = Float.cos a
  and sin_a = Float.sin a in
  MultMatrix.of_list_exn
    [ [ cos_a; -.sin_a; 0.; 10. ]; [ sin_a; cos_a; 0.; 20. ]; [ 0.; 0.; 1.; 30. ] ]

let mat_mul_cube =
  (* NOTE: These cubes should be equivalent if matmul is working correctly. *)
  let box = Model.cube ~center:true (10., 10., 10.) in
  let a = box |> Model.multmatrix mat
  and b =
    box |> Model.rotate (0., 0., Math.pi /. 4.) |> Model.translate (10., 20., 30.)
  in
  Model.union [ a; b ]

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
  Util.write (open_out "mat_mul_cube.scad") mat_mul_cube;
  print_endline "Done!"
