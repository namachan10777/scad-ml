(** {0 Linear Rounded Prism} *)
open Scad_ml

let () =
  let bot =
    Poly2.make Vec2.[ v (-4.) 0.; v 5. 3.; v 0. 7.; v 8. 7.; v 20. 20.; v 10. 0. ]
    |> Poly2.map List.rev
  in
  let scad =
    Mesh.(
      linear_prism
        ~outer:
          Prism.(
            spec
              ~joint_top:(0.25, 0.25)
              ~joint_bot:(0.25, 0.25)
              ~joint_sides:(`Flat (2.5, 2.5))
              ())
        ~height:5.
        bot)
    |> Mesh.to_scad
  in
  Scad.to_file "rounded_prism_pointy.scad" scad
