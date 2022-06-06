open Scad_ml

let () =
  let shape =
    Poly2.make Vec2.[ v (-4.) 0.; v 5. 3.; v 0. 7.; v 8. 7.; v 20. 20.; v 10. 0. ]
  in
  let scad =
    let rounded =
      Poly2.to_scad (Poly2.offset (`Radius (-0.5)) shape)
      |> Scad.linear_extrude ~height:1.
    and pointy =
      Poly2.to_scad shape
      |> Scad.linear_extrude ~height:1.
      |> Scad.translate (v3 0. 0. (-0.5))
      |> Scad.color ~alpha:0.5 Color.Silver
    in
    Scad.union [ rounded; pointy ]
  in
  Scad.to_file "offset_poly.scad" scad
