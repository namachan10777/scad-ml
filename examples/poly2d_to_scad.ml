open Scad_ml

let () =
  let scad =
    let holes =
      let s =
        Path2.square ~center:true (v2 2. 2.) |> Path2.rotate (Float.pi /. 4.) |> List.rev
      in
      Path2.[ s; translate (v2 (-2.) (-2.)) s; translate (v2 2. 2.) s ]
    and outer = Path2.square ~center:true (v2 10. 10.) in
    Poly2.(to_scad @@ make ~holes outer)
  and oc = open_out "poly2d_to_scad.scad" in
  Scad.write oc scad;
  close_out oc
