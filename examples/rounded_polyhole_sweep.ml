open Scad_ml

let () =
  let transforms =
    let bez =
      Bezier3.of_path Vec3.[ v 0. 0. 2.; v 0. 20. 20.; v 40. 10. 0.; v 50. 10. 5. ]
    in
    Path3.to_transforms ~euler:false (Bezier3.curve ~fn:20 bez)
  in
  let scad =
    let holes =
      let s = List.rev @@ Path2.circle ~fn:90 2.
      and d = 1.9 in
      Path2.[ translate (v2 (-.d) (-.d)) s; translate (v2 d d) s ]
    and outer =
      Path2.square ~center:true (v2 10. 10.)
      |> Path2.Round.(flat ~corner:(chamf (`Width 1.)))
      |> Path2.roundover
    in
    Mesh.(
      sweep
        ~transforms
        ~spec:
          Cap.(
            capped
              ~bot:(round ~holes:`Same @@ circ (`Radius (-0.6)))
              ~top:(round @@ circ (`Radius 0.5)))
        (Poly2.make ~holes outer))
    |> Mesh.merge_points
    |> Mesh.to_scad
  and oc = open_out "rounded_polyhole_sweep.scad" in
  Scad.write oc scad;
  close_out oc
