open Scad_ml

let () =
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
    Mesh.polar_plot ~r_step:0.4 ~max_r:22. rose |> Mesh.to_scad
  and oc = open_out "polar_rose.scad" in
  Scad.write oc scad;
  close_out oc
