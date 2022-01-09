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
  and step = 0.005
  and f t =
    ( ((t /. 1.5) +. 0.5) *. 100. *. Float.cos (6. *. 360. *. t *. Float.pi /. 180.)
    , ((t /. 1.5) +. 0.5) *. 100. *. Float.sin (6. *. 360. *. t *. Float.pi /. 180.)
    , 200. *. (1. -. t) )
  in
  let scad =
    let path =
      List.init (Int.of_float (1. /. step)) (fun i -> f @@ (Float.of_int i *. step))
    in
    let transforms = Sweep.transforms_of_path path in
    Sweep.sweep ~convexity:5 ~transforms shape
  and oc = open_out "ml_sweep.scad" in
  Scad.write oc scad;
  close_out oc
