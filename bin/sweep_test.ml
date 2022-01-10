open Scad_ml

let path () =
  let shape =
    [ -10., -1., 0.
    ; -10., 6., 0.
    ; -7., 6., 0.
    ; -7., 1., 0.
    ; 7., 1., 0.
    ; 7., 6., 0.
    ; 10., 6., 0.
    ; 10., -1., 0.
    ]
  and step = 0.005 in
  let f i =
    let t = Float.of_int i *. step in
    ( ((t /. 1.5) +. 0.5) *. 100. *. Float.cos (6. *. 360. *. t *. Float.pi /. 180.)
    , ((t /. 1.5) +. 0.5) *. 100. *. Float.sin (6. *. 360. *. t *. Float.pi /. 180.)
    , 200. *. (1. -. t) )
  in
  let scad =
    let path = List.init (Int.of_float (1. /. step)) f in
    let transforms = Sweep.transforms_of_path path in
    Sweep.sweep ~convexity:5 ~transforms shape
  and oc = open_out "ml_sweep.scad" in
  Scad.write oc scad;
  close_out oc

let spiral_2d () =
  let square =
    let s = 5. in
    [ -.s, -.s, 0.; -.s, s, 0.; s, s, 0.; s, -.s, 0. ]
  and step = 0.001 in
  let f i =
    let t = Float.of_int i *. step in
    MultMatrix.(
      mul
        (vector_rotation (0., 0., 1.) (t *. Float.pi *. 40.))
        (translation (10. +. (500. *. t), 0., 0.)))
  in
  let scad = Sweep.sweep ~transforms:(List.init (Int.of_float (1. /. step) + 1) f) square
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
  let shape = [ 0., 0., 0.; w, 0., 0.; w, 1., 0.; 0., 1., 0. ] in
  let f i =
    let t = Float.of_int i *. step in
    MultMatrix.(
      mul
        (mul (rotation (rad 90., 0., rad t)) (translation (r, 0., 0.)))
        (scaling (1., h +. (s *. Float.sin (rad (t *. 6.))), 1.)))
  in
  let scad = Sweep.sweep ~transforms:(List.init ((360 / 4) + 1) f) shape
  and oc = open_out "ml_wave_cylinder.scad" in
  Scad.write oc scad;
  close_out oc
