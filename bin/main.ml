let () = Printf.printf "%f" Float.pi

let () =
  Sweep_test.path ();
  Sweep_test.spiral_2d ();
  Sweep_test.wave_cylinder ();
  Sweep_test.spline_path ()
