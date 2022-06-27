(** {0 Poly2d shape contsruction} *)
open Scad_ml

let () =
  let scad =
    let holes =
      let s =
        Path2.square ~center:true (v2 2. 2.) |> Path2.rotate (Float.pi /. 4.) |> List.rev
      in
      Path2.[ s; translate (v2 (-2.) (-2.)) s; translate (v2 2. 2.) s ]
    and outer = Path2.square ~center:true (v2 10. 10.) in
    Scad.linear_extrude ~height:1. Poly2.(to_scad @@ make ~holes outer)
  in
  Scad.to_file "poly2d_to_scad.scad" scad
