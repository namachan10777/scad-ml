let () = Printf.printf "%f\n" Float.pi

let () =
  (* Sweep_test.path (); *)
  (* Sweep_test.spiral_2d (); *)
  (* Sweep_test.wave_cylinder (); *)
  Sweep_test.spline_path ();
  (* Sweep_test.bezier_path (); *)
  Sweep_test.arc_points ();
  (* Sweep_test.rounded_poly (); *)
  (* Sweep_test.polyround_basic (); *)
  (* Sweep_test.polyround_parametric (); *)
  (* Sweep_test.polyround_triangle_extrude ();
   * Sweep_test.polyround_basic_extrude ();
   * Sweep_test.polyround_sweep (); *)
  (* Sweep_test.resample_path (); *)
  (* Sweep_test.poly_linear_extrude (); *)
  (* Sweep_test.polyround_linear_extrude (); *)
  (* Sweep_test.helix_path (); *)
  (* Sweep_test.helix_sweep (); *)
  (* Sweep_test.helix_extrude (); *)
  (* Sweep_test.sweep_starburst ~euler:true; *)
  (* Sweep_test.sweep_starburst ~euler:false; *)
  (* Sweep_test.tri_mesh_poly (); *)
  (* Sweep_test.rounding_basic ();
   * Sweep_test.offset_poly ();
   * Sweep_test.offset_sweep (); *)
  (* Sweep_test.offset_linear_extrude (); *)
  (* Sweep_test.cartesian_gravity_well (); *)
  (* Sweep_test.polar_rose (); *)
  (* Sweep_test.axial_chalice (); *)
  (* Sweep_test.bezier_path (); *)
  Sweep_test.polyholes ();
  Sweep_test.poly2d_to_scad ()
