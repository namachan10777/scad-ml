open Scad_ml

let mat =
  let a = Float.pi /. 4. in
  let cos_a = Float.cos a
  and sin_a = Float.sin a in
  MultMatrix.of_row_list_exn
    [ cos_a, -.sin_a, 0., 10.; sin_a, cos_a, 0., 20.; 0., 0., 1., 30. ]

let mat_mul_cube =
  (* NOTE: These cubes should be equivalent if matmul is working correctly. *)
  let box = Model.cube ~center:true (10., 10., 10.) in
  let a = Model.multmatrix mat box
  and b =
    box |> Model.rotate (0., 0., Float.pi /. 4.) |> Model.translate (10., 20., 30.)
  in
  Model.union [ a; b ]

let quat_cube =
  (* NOTE: These cubes should be equivalent if quaternion is working correctly. *)
  let box = Model.cube ~center:true (10., 10., 10.)
  and angle = Float.pi /. 4.
  and ax = 1., 1., 0. in
  let a = Model.vector_rotate ax angle box
  and b = Model.multmatrix Quaternion.(to_multmatrix (make ax angle)) box in
  Model.union [ a; b ]

let quat_slerp =
  let cyl = Model.cylinder ~center:true 2.5 20. in
  let q0 = Quaternion.make (0., 1., 0.) 0. in
  let q1 = Quaternion.make (0., 1., 0.) (Float.pi /. 2.) in
  let slerp = Quaternion.slerp q0 q1 in
  let step t scad =
    Model.quaternion (slerp t) scad |> Model.translate (0., 30. *. t, 0.)
  in
  Model.union
    [ cyl; step 0.1 cyl; step 0.5 cyl; step 0.7 cyl; step 0.90 cyl; step 1. cyl ]

let square = Model.square ~center:true (10., 10.)
let circle = Model.circle 10.
let triangle_polygon = Model.polygon [ -0.5, 0.; 0., 1.; 0.5, 0. ]
let linear_extrude_circle = Model.linear_extrude ~height:10. circle

let rotate_extrude_triangle =
  Model.rotate_extrude (Model.translate (3., 0., 0.) triangle_polygon)

let hello = Model.text "Hello, world!"

let vertical_text =
  Model.text "Tall Text" ~spacing:5. ~valign:Text.Top ~direction:Text.TopToBottom

let () =
  print_endline "Building test scads...";
  Scad.write (open_out "square.scad") square;
  Scad.write (open_out "circle.scad") circle;
  Scad.write (open_out "triangle_polygon.scad") triangle_polygon;
  Scad.write (open_out "linear_extrude_circle.scad") linear_extrude_circle;
  Scad.write (open_out "rotate_extrude_triangle.scad") rotate_extrude_triangle;
  Scad.write (open_out "hello.scad") hello;
  Scad.write (open_out "vertical_text.scad") vertical_text;
  Scad.write (open_out "mat_mul_cube.scad") mat_mul_cube;
  Scad.write (open_out "quat_cube.scad") quat_cube;
  Scad.write (open_out "quat_slerp.scad") quat_slerp;
  print_endline "Done!"
