open Scad_ml

let () =
  let r = 10.
  and h = 20.
  and w = 2.
  and s = 2.
  and step = 4.
  and rad d = d *. Float.pi /. 180. in
  let shape = Poly2.make [ v2 0. 0.; v2 w 0.; v2 w 1.; v2 0. 1. ] in
  let f i =
    let t = Float.of_int i *. step in
    MultMatrix.(
      mul
        (mul (rotation (v3 (rad 90.) 0. (rad t))) (translation (v3 r 0. 0.)))
        (scaling (v3 1. (h +. (s *. Float.sin (rad (t *. 6.)))) 1.)))
  in
  let scad = Mesh.(to_scad @@ sweep ~transforms:(List.init ((360 / 4) + 1) f) shape)
  and oc = open_out "wave_cylinder.scad" in
  Scad.write oc scad;
  close_out oc
