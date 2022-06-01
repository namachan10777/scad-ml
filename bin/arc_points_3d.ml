open Scad_ml

let () =
  let arc = Path3.arc_through ~fn:5 (v3 10. 10. 0.) (v3 20. 20. 10.) (v3 10. 30. 20.) in
  let scad =
    List.mapi
      (fun i { x; y; z } ->
        Scad.text ~size:5. (Printf.sprintf "%i" i)
        |> Scad.color Color.Red
        |> Scad.linear_extrude ~height:1.
        |> Scad.translate { x; y; z } )
      arc
    |> Scad.union
  and oc = open_out "arc_points_3d.scad" in
  Scad.write oc scad;
  close_out oc
