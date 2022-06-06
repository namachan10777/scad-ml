open Scad_ml

let () =
  let arc = Path2.arc_through ~fn:5 (v2 10. 10.) (v2 20. 20.) (v2 10. 30.) in
  let scad =
    List.mapi
      (fun i p ->
        Scad.text ~size:5. (Printf.sprintf "%i" i)
        |> Scad.color Color.Red
        |> Scad.translate p )
      arc
    |> Scad.union
  in
  Scad.to_file "arc_points_2d.scad" scad
