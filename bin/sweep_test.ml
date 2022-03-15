open Scad_ml

let elbow_shape =
  [ v2 (-10.) (-1.)
  ; v2 (-10.) 6.
  ; v2 (-7.) 6.
  ; v2 (-7.) 1.
  ; v2 7. 1.
  ; v2 7. 6.
  ; v2 10. 6.
  ; v2 10. (-1.)
  ]

let path () =
  let step = 0.005 in
  let f i =
    let t = Float.of_int i *. step in
    let x = ((t /. 1.5) +. 0.5) *. 100. *. Float.cos (6. *. 360. *. t *. Float.pi /. 180.)
    and y = ((t /. 1.5) +. 0.5) *. 100. *. Float.sin (6. *. 360. *. t *. Float.pi /. 180.)
    and z = 200. *. (1. -. t) in
    v3 x y z
  in
  let scad =
    let path = List.init (Int.of_float (1. /. step)) f in
    let transforms = Path3d.to_transforms path in
    Poly3d.(to_scad @@ sweep ~transforms elbow_shape)
  and oc = open_out "ml_sweep_path.scad" in
  Scad.write oc scad;
  close_out oc

let spiral_2d () =
  let square = Poly2d.square ~center:true (v2 10. 10.)
  and step = 0.001 in
  let f i =
    let t = Float.of_int i *. step in
    MultMatrix.(
      mul
        (vector_rotation (v3 0. 0. 1.) (t *. Float.pi *. 40.))
        (translation (v3 (10. +. (500. *. t)) 0. 0.)))
  in
  let scad =
    Poly3d.sweep ~transforms:(List.init (Int.of_float (1. /. step) + 1) f) square
    |> Poly3d.to_scad
  and oc = open_out "ml_spiral.scad" in
  Scad.write oc scad;
  close_out oc

let wave_cylinder () =
  let r = 10.
  and h = 20.
  and w = 2.
  and s = 2.
  and step = 4.
  and rad d = d *. Float.pi /. 180. in
  let shape = [ v2 0. 0.; v2 w 0.; v2 w 1.; v2 0. 1. ] in
  let f i =
    let t = Float.of_int i *. step in
    MultMatrix.(
      mul
        (mul (rotation (v3 (rad 90.) 0. (rad t))) (translation (v3 r 0. 0.)))
        (scaling (v3 1. (h +. (s *. Float.sin (rad (t *. 6.)))) 1.)))
  in
  let scad = Poly3d.(to_scad @@ sweep ~transforms:(List.init ((360 / 4) + 1) f) shape)
  and oc = open_out "ml_wave_cylinder.scad" in
  Scad.write oc scad;
  close_out oc

let spline_path () =
  let control_pts = [ v2 0. 10.; v2 10. 40.; v2 20. 40.; v2 30. (-20.); v2 40. (-40.) ]
  and square = Poly2d.square ~center:true (v2 2. 5.) in
  let marks =
    let s = Scad.color Color.Red @@ Scad.sphere 2. in
    List.map (fun { x; y } -> Scad.translate (v3 x y 0.) s) control_pts
  and line =
    let path = CubicSpline.(path_to_3d @@ interpolate_path (fit control_pts) 100) in
    Poly3d.(to_scad @@ sweep ~transforms:(Path3d.to_transforms ~euler:false path) square)
  in
  let scad = Scad.union (line :: marks)
  and oc = open_out "spline.scad" in
  Scad.write oc scad;
  close_out oc

let arc_points () =
  let arc = Path2d.arc_through ~fn:5 (v2 10. 10.) (v2 20. 20.) (v2 10. 30.) in
  let scad =
    List.mapi
      (fun i { x; y } ->
        Scad.text ~size:5. (Printf.sprintf "%i" i)
        |> Scad.color Color.Red
        |> Scad.translate (v3 x y 0.) )
      arc
    |> Scad.union
  and oc = open_out "arc_points.scad" in
  Scad.write oc scad;
  close_out oc

let arc_points_3d () =
  let arc = Path3d.arc_through ~fn:5 (v3 10. 10. 0.) (v3 20. 20. 10.) (v3 10. 30. 20.) in
  let scad =
    List.mapi
      (fun i { x; y; z } ->
        Scad.text ~size:5. (Printf.sprintf "%i" i)
        |> Scad.color Color.Red
        |> Scad.translate { x; y; z } )
      arc
    |> Scad.union
  and oc = open_out "arc_points_3d.scad" in
  Scad.write oc scad;
  close_out oc

let rounded_poly () =
  let radii_pts = [ v3 0. 0. 0.5; v3 10. 0. 0.5; v3 0. 10. 0.5 ] in
  let scad = Scad.polygon (PolyRound.polyround ~fn:10 radii_pts)
  and oc = open_out "polyround_triangle_ml.scad" in
  Scad.write oc scad;
  close_out oc

let polyround_basic () =
  let radii_pts =
    [ v3 (-4.) 0. 1.
    ; v3 5. 3. 1.5
    ; v3 0. 7. 0.1
    ; v3 8. 7. 10.
    ; v3 20. 20. 0.8
    ; v3 10. 0. 10.
    ]
  in
  let scad =
    let rounded =
      Scad.polygon (PolyRound.polyround ~fn:30 radii_pts)
      |> Scad.linear_extrude ~height:1.
    and pointy =
      Scad.polygon (List.map Vec2.of_vec3 radii_pts)
      |> Scad.linear_extrude ~height:1.
      |> Scad.translate (v3 0. 0. (-0.5))
      |> Scad.color ~alpha:0.5 Color.Silver
    in
    Scad.union [ rounded; pointy ]
  and oc = open_out "polyround_basic_ml.scad" in
  Scad.write oc scad;
  close_out oc

let polyround_parametric () =
  let w = 20.
  and h = 25.
  and slot_w = 8.
  and slot_h = 15.
  and slot_pos = 8.
  and min_r = 1.5
  and far_corner_r = 6.
  and internal_r = 3. in
  let radii_pts =
    [ v3 0. 0. far_corner_r
    ; v3 0. h min_r
    ; v3 slot_pos h min_r
    ; v3 slot_pos (h -. slot_h) internal_r
    ; v3 (slot_pos +. slot_w) (h -. slot_h) internal_r
    ; v3 (slot_pos +. slot_w) h min_r
    ; v3 w h min_r
    ; v3 w 0. min_r
    ]
  in
  let scad =
    let rounded =
      Scad.polygon (PolyRound.polyround ~fn:10 radii_pts)
      |> Scad.linear_extrude ~height:1.
    and pointy =
      Scad.polygon (List.map Vec2.of_vec3 radii_pts)
      |> Scad.linear_extrude ~height:1.
      |> Scad.translate (v3 0. 0. (-0.5))
      |> Scad.color ~alpha:0.5 Color.Silver
    in
    Scad.union [ rounded; pointy ]
  and oc = open_out "polyround_parametric_ml.scad" in
  Scad.write oc scad;
  close_out oc

let polyround_triangle_extrude () =
  let radii_pts = [ v3 0. 0. 0.5; v3 10. 0. 0.5; v3 0. 10. 0.5 ] in
  let scad =
    PolyRound.polyround_extrude ~fn:4 ~height:4. ~r1:1. ~r2:1. radii_pts |> Poly3d.to_scad
  and oc = open_out "polyround_triangle_extrude_ml.scad" in
  Scad.write oc scad;
  close_out oc

let polyround_basic_extrude () =
  let radii_pts =
    [ v3 (-4.) 0. 1.
    ; v3 5. 3. 1.5
    ; v3 0. 7. 0.1
    ; v3 8. 7. 10.
    ; v3 20. 20. 0.8
    ; v3 10. 0. 10.
    ]
  in
  let scad =
    PolyRound.polyround_extrude ~fn:10 ~height:0.1 ~r1:(-1.) ~r2:1. radii_pts
    |> Poly3d.to_scad
  and oc = open_out "polyround_basic_extrude_ml.scad" in
  Scad.write oc scad;
  close_out oc

let polyround_sweep () =
  let radii_pts = [ v3 0. 0. 0.5; v3 10. 0. 0.5; v3 0. 10. 0.5 ]
  and step = 0.005 in
  let f i =
    let t = Float.of_int i *. step in
    let x = ((t /. 1.5) +. 0.5) *. 100. *. Float.cos (6. *. 360. *. t *. Float.pi /. 180.)
    and y = ((t /. 1.5) +. 0.5) *. 100. *. Float.sin (6. *. 360. *. t *. Float.pi /. 180.)
    and z = 200. *. (1. -. t) in
    v3 x y z
  in
  let scad =
    let path = List.init (Int.of_float (1. /. step)) f in
    let transforms = Path3d.to_transforms ~euler:false path in
    PolyRound.polyround_sweep ~fn:6 ~r1:2. ~r2:2. ~transforms radii_pts |> Poly3d.to_scad
  and oc = open_out "polyround_sweep.scad" in
  Scad.write oc scad;
  close_out oc

let resample_path () =
  let path = [ v3 0. 0. 0.; v3 5. 5. 5.; v3 5. 5. 15. ] in
  (* let resampled = Path3d.resample ~freq:(`N 10) path in *)
  let resampled = Path3d.resample ~freq:(`Spacing 1.) path in
  let old_marks =
    let s = Scad.color Color.Red @@ Scad.sphere 0.5 in
    List.map (fun p -> Scad.translate p s) path
  and new_marks =
    let s = Scad.color ~alpha:0.1 Color.Yellow @@ Scad.sphere 1.0 in
    List.map (fun p -> Scad.translate p s) resampled
  in
  let scad = Scad.union (old_marks @ new_marks)
  and oc = open_out "resampled_path.scad" in
  Scad.write oc scad;
  close_out oc

let poly_linear_extrude () =
  let scad =
    Poly2d.square ~center:true (v2 3. 3.)
    |> Poly3d.linear_extrude
         ~slices:100
         ~scale:(v2 4. 4.)
         ~twist:(2. *. Float.pi)
         ~center:false
         ~height:10.
    |> Poly3d.to_scad
  and oc = open_out "poly_linear_extrude.scad" in
  Scad.write oc scad;
  close_out oc

let polyround_linear_extrude () =
  let scad =
    Poly2d.square ~center:true (v2 3. 3.)
    |> List.map (Vec3.of_vec2 ~z:0.5)
    |> List.map (Vec3.translate (v3 1.5 1.5 0.))
    |> PolyRound.polyround_extrude
         ~slices:100
         ~fn:10
         ~cap_fn:30
         ~scale:(v2 4. 4.)
         ~twist:(2. *. Float.pi)
         ~center:false
         ~r1:0.5
         ~r2:1.4
         ~height:10.
    |> Poly3d.to_scad
  and oc = open_out "polyround_linear_extrude.scad" in
  Scad.write oc scad;
  close_out oc

let helix_path () =
  let scad =
    let pts = Path3d.helix ~left:false ~pitch:5. ~n_turns:10 ~r2:10. 5. in
    let s = Scad.color Color.Red @@ Scad.sphere 1. in
    Scad.union @@ List.map (fun p -> Scad.translate p s) pts
  and oc = open_out "helix_path.scad" in
  Scad.write oc scad;
  close_out oc

let helix_sweep () =
  let scad =
    let path = Path3d.helix ~left:true ~pitch:30. ~n_turns:10 ~r2:100. 50. in
    (* let transforms = Path3d.to_transforms ~euler:true path in *)
    let transforms =
      Path3d.to_transforms ~twist:(-240. /. 180. *. Float.pi) ~euler:false path
    in
    Poly3d.(to_scad @@ sweep ~transforms elbow_shape)
  and oc = open_out "helix_sweep.scad" in
  Scad.write oc scad;
  close_out oc

let helix_extrude () =
  let scad =
    Poly3d.helix_extrude
      ~scale:(v2 1. 1.)
      ~left:true
      ~pitch:30.
      ~n_turns:10
      ~r2:100.
      50.
      elbow_shape
    |> Poly3d.to_scad
  and oc = open_out "helix_extrude.scad" in
  Scad.write oc scad;
  close_out oc

let sweep_starburst ~euler =
  let scad =
    let paths =
      let d = 20.
      and p { x; y; z } = [ v3 x y z; v3 (x *. 2.) (y *. 2.) (z *. 2.) ] in
      let out = [ v3 d 0. 0.; v3 d 0. d; v3 d 0. (-.d) ] in
      let f i =
        List.map (fun s -> Vec3.rotate (v3 0. 0. Float.(pi /. 4. *. i)) s |> p) out
      in
      p (v3 0. 0. d) :: p (v3 0. 0. (-.d)) :: List.concat_map f (List.init 8 Float.of_int)
    in
    let flat = Scad.polygon elbow_shape |> Scad.linear_extrude ~height:1.
    and f path =
      let transforms = Path3d.to_transforms ~euler path in
      Poly3d.sweep ~transforms elbow_shape |> Poly3d.to_scad
    in
    Scad.union @@ (flat :: List.map f paths)
  and oc =
    open_out
      (Printf.sprintf "sweep_starburst_%s.scad" (if euler then "euler" else "standard"))
  in
  Scad.write oc scad;
  close_out oc

let tri_mesh_poly () =
  let scad =
    List.init 10 (fun y ->
        let y = y + 1 in
        List.init y (fun x -> Float.(v3 (of_int x) (of_int y) (of_int y))) )
    |> Poly3d.tri_mesh
    |> Poly3d.to_scad
  and oc = open_out "tri_array.scad" in
  Scad.write oc scad;
  close_out oc

let rounding_basic () =
  let shape = Vec2.[ v (-4.) 0.; v 5. 3.; v 0. 7.; v 8. 7.; v 20. 20.; v 10. 0. ] in
  let shape_spec =
    let radii = [ 1.; 1.5; 0.1; 10.; 0.8; 10. ] in
    Rounding2d.(mix (List.map2 (fun p r -> p, Some (circ (`Radius r))) shape radii))
    (* Rounding2d.(flat ~spec:(circ (`Joint 2.))) shape *)
    (* Rounding2d.(flat ~spec:(circ (`Cut 0.5))) shape *)
    (* Rounding2d.(flat ~spec:(chamf (`Width 0.5))) shape *)
    (* Rounding2d.(flat ~spec:(bez ~curv:0.8 (`Joint 1.))) shape *)
  in
  let scad =
    let rounded =
      Scad.polygon (Rounding2d.corners ~fn:30 shape_spec)
      |> Scad.linear_extrude ~height:1.
    and pointy =
      Scad.polygon shape
      |> Scad.linear_extrude ~height:1.
      |> Scad.translate (v3 0. 0. (-0.5))
      |> Scad.color ~alpha:0.5 Color.Silver
    in
    Scad.union [ rounded; pointy ]
  and oc = open_out "rounding_basic_ml.scad" in
  Scad.write oc scad;
  close_out oc

let offset_poly () =
  let shape = Vec2.[ v (-4.) 0.; v 5. 3.; v 0. 7.; v 8. 7.; v 20. 20.; v 10. 0. ] in
  let scad =
    let rounded =
      Scad.polygon (Poly2d.offset (`Radius (-0.5)) shape)
      |> Scad.linear_extrude ~height:1.
    and pointy =
      Scad.polygon shape
      |> Scad.linear_extrude ~height:1.
      |> Scad.translate (v3 0. 0. (-0.5))
      |> Scad.color ~alpha:0.5 Color.Silver
    in
    Scad.union [ rounded; pointy ]
  and oc = open_out "offset_poly.scad" in
  Scad.write oc scad;
  close_out oc

let offset_sweep () =
  let shape = Poly2d.square ~center:true (v2 3. 3.) in
  let shape_spec =
    (* Rounding2d.(flat ~spec:(circ (`Joint 2.))) shape *)
    Rounding2d.(flat ~spec:(circ (`Cut 0.5))) shape
    (* Rounding2d.(flat ~spec:(chamf (`Width 0.5))) shape *)
    (* Rounding2d.(flat ~spec:(bez ~curv:0.8 (`Joint 3.))) shape *)
  in
  let transforms =
    let step = 0.005 in
    let f i =
      let t = Float.of_int i *. step in
      let x =
        ((t /. 1.5) +. 0.5) *. 100. *. Float.cos (6. *. 360. *. t *. Float.pi /. 180.)
      and y =
        ((t /. 1.5) +. 0.5) *. 100. *. Float.sin (6. *. 360. *. t *. Float.pi /. 180.)
      and z = 200. *. (1. -. t) in
      v3 x y z
    in
    let path = List.init (Int.of_float (1. /. step)) f in
    Path3d.to_transforms ~euler:false path
  in
  let scad =
    Rounding2d.corners ~fn:30 shape_spec
    |> RoundExtrude.(
         sweep (* ~mode:`Chamfer *)
           ~transforms
             (* ~bot:(circ (`Radius (-1.))) *)
             (* ~top:(circ (`Radius 0.1))) *)
           ~bot:(tear (`Radius (-1.)))
           ~top:(tear (`Radius 0.5)))
    (* ~bot:(chamf (`Cut 0.2) (`Angle (Float.pi /. 4.))) *)
    (* ~top:(chamf (`Cut 0.2) (`Angle (Float.pi /. 4.)))) *)
    (* ~bot:(bez (`Joint 0.2)) *)
    (* ~top:(bez (`Joint 0.2))) *)
    |> Poly3d.to_scad
  and oc = open_out "offset_sweep.scad" in
  Scad.write oc scad;
  close_out oc

let offset_linear_extrude () =
  let scad =
    let shape = Poly2d.square ~center:true (v2 3. 3.) in
    Rounding2d.(corners ~fn:30 (flat ~spec:(chamf (`Cut 0.5)) shape))
    |> List.map (Vec2.translate (v2 1.5 1.5))
    |> RoundExtrude.linear_extrude
         ~slices:100
         ~fn:16
         ~scale:(v2 4. 4.)
         ~twist:(2. *. Float.pi)
         ~center:false
         ~bot:(RoundExtrude.circ (`Radius (-0.2)))
         ~top:(RoundExtrude.bez (`Cut 0.1)) (* ~top:(RoundExtrude.chamf ~cut:(-0.1) ()) *)
         ~height:10.
    |> Poly3d.to_scad
  and oc = open_out "offset_linear_extrude.scad" in
  Scad.write oc scad;
  close_out oc

let bezier_path () =
  let control_pts = Vec2.[ v 0. 10.; v 10. 40.; v 20. 40.; v 30. (-20.); v 40. (-40.) ]
  and square = Poly2d.square ~center:true (v2 2. 2.) in
  let marks =
    let s = Scad.color Color.Red @@ Scad.sphere 2. in
    List.map (fun { x; y } -> Scad.translate (v3 x y 0.) s) control_pts
  and line =
    let path = Bezier2d.(List.map Vec2.to_vec3 @@ curve ~fn:100 (of_path control_pts)) in
    Poly3d.(to_scad @@ sweep ~transforms:(Path3d.to_transforms ~euler:false path) square)
  in
  let scad = Scad.union (line :: marks)
  and oc = open_out "bezier_path.scad" in
  Scad.write oc scad;
  close_out oc

let cartesian_gravity_well () =
  let scad =
    let gravity_well ~x ~y =
      let z = 50. -. (50. /. Float.sqrt ((x *. x) +. (y *. y))) in
      if z < 1. then 1. else z
    in
    Poly3d.cartesian_plot
      ~min_x:(-10.)
      ~x_steps:30
      ~max_x:0.
      ~min_y:(-10.)
      ~y_steps:60
      ~max_y:10.
      gravity_well
    |> Poly3d.to_scad
  and oc = open_out "cartesian_gravity_well.scad" in
  Scad.write oc scad;
  close_out oc

let polar_rose () =
  let scad =
    let rose ~r ~a =
      let open Float in
      let x =
        pow
          ( (r *. cos a *. cos (r *. 8. *. pi /. 180.))
          +. (r *. sin a *. sin (r *. 35. *. pi /. 180.)) )
          2.
        /. -300.
      in
      ((15. +. (5. *. sin (r *. 10. *. pi /. 180.))) *. exp x) +. 1.
    in
    Poly3d.polar_plot ~r_step:0.4 ~max_r:22. rose |> Poly3d.to_scad
  and oc = open_out "polar_rose.scad" in
  Scad.write oc scad;
  close_out oc

let axial_chalice () =
  let scad =
    let f ~z ~a:_ = Float.(5. *. (cos (log ((z /. 5.) +. 1.) *. pi) +. 2.)) in
    let outer = Poly3d.axial_plot ~min_z:0. ~z_steps:50 ~max_z:50. f
    and inner =
      Poly3d.axial_plot ~min_z:2. ~z_steps:50 ~max_z:51. (fun ~z ~a -> f ~z ~a -. 2.)
    in
    Scad.difference (Poly3d.to_scad outer) [ Poly3d.to_scad inner ]
  and oc = open_out "axial_chalice.scad" in
  Scad.write oc scad;
  close_out oc

let polyholes () =
  let scad =
    let holes =
      let s = Poly2d.square ~center:true (v2 2. 2.) |> Poly2d.rotate (Float.pi /. 4.) in
      Poly2d.[ s; translate (v2 (-2.) (-2.)) s; translate (v2 2. 2.) s ]
    and outer = List.rev @@ Poly2d.square ~center:true (v2 10. 10.) in
    let poly =
      Poly3d.polyhole_partition
        ~holes:(List.map (List.map Vec3.of_vec2) holes)
        (List.map Vec3.of_vec2 outer)
      |> Poly3d.to_scad
      |> Scad.color ~alpha:0.5 Color.Silver
    and reference =
      Scad.difference (Scad.polygon outer) (List.map Scad.polygon holes)
      |> Scad.linear_extrude ~height:1.
      |> Scad.translate (v3 0. 0. (-3.))
      |> Scad.color ~alpha:0.5 Color.BlueViolet
    in
    Scad.union [ poly; reference ]
  and oc = open_out "polyholes.scad" in
  Scad.write oc scad;
  close_out oc

let poly2d_to_scad () =
  let scad =
    let holes =
      let s = Poly2d.square ~center:true (v2 2. 2.) |> Poly2d.rotate (Float.pi /. 4.) in
      Poly2d.[ s; translate (v2 (-2.) (-2.)) s; translate (v2 2. 2.) s ]
    and outer = List.rev @@ Poly2d.square ~center:true (v2 10. 10.) in
    Poly2d.to_scad ~holes outer
  and oc = open_out "poly2d_to_scad.scad" in
  Scad.write oc scad;
  close_out oc

let rounded_polyhole_sweep () =
  let transforms =
    let bez =
      Bezier3d.of_path Vec3.[ v 0. 0. 2.; v 0. 20. 20.; v 40. 10. 0.; v 50. 10. 5. ]
    in
    Path3d.to_transforms ~euler:false (Bezier3d.curve ~fn:20 bez)
  in
  let scad =
    let holes =
      let s = Poly2d.circle ~fn:90 2.
      and d = 2. in
      Poly2d.(
        RoundExtrude.
          [ hole ~bot:(Some `Same) @@ translate (v2 (-.d) (-.d)) s
          ; hole ~bot:(Some `Same) @@ translate (v2 d d) s
          ])
    and outer =
      Poly2d.square ~center:true (v2 10. 10.)
      |> Rounding2d.(flat ~spec:(chamf (`Width 1.)))
      |> Rounding2d.corners
    in
    RoundExtrude.(
      sweep
        ~transforms
        ~bot:(circ (`Radius (-0.8)))
        ~top:(circ (`Radius 0.5))
        ~holes
        outer)
    (* |> Poly3d.merge_points *)
    |> Poly3d.to_scad
  and oc = open_out "rounded_polyhole_sweep.scad" in
  Scad.write oc scad;
  close_out oc

let polytext () =
  let scad =
    let font = "Fira Code" in
    let PolyText.{ outer; inner } = PolyText.glyph_outline ~font 'g' in
    let Path2d.{ min = { x = l; y = b }; max = { x = r; y = t } } = Path2d.bbox outer in
    Printf.printf "left = %.2f; right = %.2f; top = %.2f; bot = %.2f\n" l r t b;
    Scad.union
      [ Poly2d.to_scad ~holes:inner outer
      ; Scad.text ~font ~size:8. "g" |> Scad.color ~alpha:0.5 Color.BlueViolet
      ]
  and oc = open_out "polytext.scad" in
  Scad.write oc scad;
  close_out oc

let rounded_prism_cube () =
  let bot = Poly2d.square ~center:true (v2 5. 5.) |> List.rev_map Vec2.to_vec3 in
  let top =
    Path3d.translate (v3 0. 0. 5.) (Path3d.rotate (v3 0. (Float.pi /. 4.) 0.) bot)
  in
  let scad =
    RoundExtrude.prism
      ~joint_top:(1., 1.)
      ~joint_bot:(1., 1.)
      ~joint_sides:(`Flat (1.5, 1.5))
      bot
      top
    |> Poly3d.to_scad
  and oc = open_out "rounded_prism_cube.scad" in
  Scad.write oc scad;
  close_out oc

let rounded_prism_pointy () =
  let bot =
    Vec3.[ v (-4.) 0. 0.; v 5. 3. 0.; v 0. 7. 0.; v 8. 7. 0.; v 20. 20. 0.; v 10. 0. 0. ]
  in
  let top = Path3d.translate (v3 0. 0. 5.) bot in
  let scad =
    RoundExtrude.prism
      ~joint_top:(0.25, 0.25)
      ~joint_bot:(0.25, 0.25)
      ~joint_sides:(`Flat (2.5, 2.5))
      bot
      top
    |> Poly3d.to_scad
  and oc = open_out "rounded_prism_pointy.scad" in
  Scad.write oc scad;
  close_out oc
