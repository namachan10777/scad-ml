open Scad_ml

let () =
  let shape = Vec2.[ v (-4.) 0.; v 5. 3.; v 0. 7.; v 8. 7.; v 20. 20.; v 10. 0. ] in
  let shape_spec =
    let radii = [ 1.; 1.5; 0.1; 10.; 0.8; 10. ] in
    Path2.Round.circles ~kind:`Radius (List.combine shape radii)
  in
  let scad =
    let rounded =
      Scad.polygon (Path2.roundover ~fn:30 shape_spec) |> Scad.linear_extrude ~height:1.
    and pointy =
      Scad.polygon shape
      |> Scad.linear_extrude ~height:1.
      |> Scad.translate (v3 0. 0. (-0.5))
      |> Scad.color ~alpha:0.5 Color.Silver
    in
    Scad.union [ rounded; pointy ]
  and oc = open_out "circular_rounding.scad" in
  Scad.write oc scad;
  close_out oc
