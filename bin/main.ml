let () = Printf.printf "%f\n" Float.pi

let () =
  Sweep_test.path ();
  Sweep_test.spiral_2d ();
  Sweep_test.wave_cylinder ();
  Sweep_test.spline_path ();
  Sweep_test.bezier_path ();
  Sweep_test.arc_points ()
