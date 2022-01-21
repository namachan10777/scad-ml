let () = Printf.printf "%f\n" Float.pi

let () =
  Sweep_test.path ();
  Sweep_test.spiral_2d ();
  Sweep_test.wave_cylinder ();
  Sweep_test.spline_path ();
  Sweep_test.bezier_path ();
  Sweep_test.arc_points ();
  Sweep_test.rounded_poly ();
  Sweep_test.polyround_basic ();
  Sweep_test.polyround_parametric ();
  Sweep_test.polyround_triangle_extrude ();
  Sweep_test.polyround_basic_extrude ();
  Sweep_test.extrude_square_with_radius ()
