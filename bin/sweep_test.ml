open Scad_ml

let path () =
  let shape = [ -10., -1.; -10., 6.; -7., 6.; -7., 1.; 7., 1.; 7., 6.; 10., 6.; 10., -1. ]
  and step = 0.005 in
  let f i =
    let t = Float.of_int i *. step in
    ( ((t /. 1.5) +. 0.5) *. 100. *. Float.cos (6. *. 360. *. t *. Float.pi /. 180.)
    , ((t /. 1.5) +. 0.5) *. 100. *. Float.sin (6. *. 360. *. t *. Float.pi /. 180.)
    , 200. *. (1. -. t) )
  in
  let scad =
    let path = List.init (Int.of_float (1. /. step)) f in
    let transforms = Poly3d.transforms_of_path path in
    Poly3d.sweep ~convexity:5 ~transforms shape
  and oc = open_out "ml_sweep_path.scad" in
  Scad.write oc scad;
  close_out oc

let spiral_2d () =
  let square = Poly2d.square ~center:true (10., 10.)
  and step = 0.001 in
  let f i =
    let t = Float.of_int i *. step in
    MultMatrix.(
      mul
        (vector_rotation (0., 0., 1.) (t *. Float.pi *. 40.))
        (translation (10. +. (500. *. t), 0., 0.)))
  in
  let scad = Poly3d.sweep ~transforms:(List.init (Int.of_float (1. /. step) + 1) f) square
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
  let shape = [ 0., 0.; w, 0.; w, 1.; 0., 1. ] in
  let f i =
    let t = Float.of_int i *. step in
    MultMatrix.(
      mul
        (mul (rotation (rad 90., 0., rad t)) (translation (r, 0., 0.)))
        (scaling (1., h +. (s *. Float.sin (rad (t *. 6.))), 1.)))
  in
  let scad = Poly3d.sweep ~transforms:(List.init ((360 / 4) + 1) f) shape
  and oc = open_out "ml_wave_cylinder.scad" in
  Scad.write oc scad;
  close_out oc

let spline_path () =
  let control_pts = [ 0., 10.; 10., 40.; 20., 40.; 30., -20.; 40., -40. ]
  and square = Poly2d.square ~center:true (0.5, 0.5) in
  let marks =
    let s = Scad.color Color.Red @@ Scad.sphere 1. in
    List.map (fun (x, y) -> Scad.translate (x, y, 0.) s) control_pts
  and line =
    let path = CubicSpline.(path_to_3d @@ interpolate_path (fit control_pts) 100) in
    Poly3d.sweep ~transforms:(Poly3d.transforms_of_path path) square
  in
  let scad = Scad.union (line :: marks)
  and oc = open_out "spline.scad" in
  Scad.write oc scad;
  close_out oc

let bezier_path () = ()

let arc_points () =
  let arc = Poly2d.arc ~fn:5 (10., 10.) (20., 20.) (10., 30.) in
  let scad =
    List.mapi
      (fun i (x, y) ->
        Scad.text ~size:5. (Printf.sprintf "%i" i)
        |> Scad.color Color.Red
        |> Scad.translate (x, y, 0.) )
      arc
    |> Scad.union
  and oc = open_out "arc_points.scad" in
  Scad.write oc scad;
  close_out oc

let rounded_poly () =
  let radii_pts = [ 0., 0., 0.5; 10., 0., 0.5; 0., 10., 0.5 ] in
  let scad = Scad.polygon (PolyRound.poly_round ~fn:10 radii_pts)
  and oc = open_out "polyround_triangle_ml.scad" in
  Scad.write oc scad;
  close_out oc

let polyround_basic () =
  let radii_pts =
    [ -4., 0., 1.; 5., 3., 1.5; 0., 7., 0.1; 8., 7., 10.; 20., 20., 0.8; 10., 0., 10. ]
  in
  let scad =
    let rounded =
      Scad.polygon (PolyRound.poly_round ~fn:30 radii_pts)
      |> Scad.linear_extrude ~height:1.
    and pointy =
      Scad.polygon (List.map Vec2.of_vec3 radii_pts)
      |> Scad.linear_extrude ~height:1.
      |> Scad.translate (0., 0., -0.5)
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
    [ 0., 0., far_corner_r
    ; 0., h, min_r
    ; slot_pos, h, min_r
    ; slot_pos, h -. slot_h, internal_r
    ; slot_pos +. slot_w, h -. slot_h, internal_r
    ; slot_pos +. slot_w, h, min_r
    ; w, h, min_r
    ; w, 0., min_r
    ]
  in
  let scad =
    let rounded =
      Scad.polygon (PolyRound.poly_round ~fn:10 radii_pts)
      |> Scad.linear_extrude ~height:1.
    and pointy =
      Scad.polygon (List.map Vec2.of_vec3 radii_pts)
      |> Scad.linear_extrude ~height:1.
      |> Scad.translate (0., 0., -0.5)
      |> Scad.color ~alpha:0.5 Color.Silver
    in
    Scad.union [ rounded; pointy ]
  and oc = open_out "polyround_parametric_ml.scad" in
  Scad.write oc scad;
  close_out oc

let polyround_triangle_extrude () =
  let radii_pts = [ 0., 0., 0.5; 10., 0., 0.5; 0., 10., 0.5 ] in
  let scad = PolyRound.poly_round_extrude ~fn:4 ~h:4. ~r1:1. ~r2:1. radii_pts
  and oc = open_out "polyround_triangle_extrude_ml.scad" in
  Scad.write oc scad;
  close_out oc

let polyround_basic_extrude () =
  let radii_pts =
    [ -4., 0., 1.; 5., 3., 1.5; 0., 7., 0.1; 8., 7., 10.; 20., 20., 0.8; 10., 0., 10. ]
  in
  let scad = PolyRound.poly_round_extrude ~fn:10 ~h:0. ~r1:(-1.) ~r2:1. radii_pts
  and oc = open_out "polyround_basic_extrude_ml.scad" in
  Scad.write oc scad;
  close_out oc

let extrude_square_with_radius () =
  let scad =
    Poly3d.extrude_with_radius ~fn:60 ~height:2. ~r1:0.2 ~r2:0.2 (Scad.square (5., 5.))
  and oc = open_out "extrude_square_with_radius_ml.scad" in
  Scad.write oc scad;
  close_out oc
