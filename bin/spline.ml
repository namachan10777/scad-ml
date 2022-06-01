open Scad_ml

let () =
  let control_pts = [ v2 0. 10.; v2 10. 40.; v2 20. 40.; v2 30. (-20.); v2 40. (-40.) ]
  and square = Poly2.square ~center:true (v2 2. 5.) in
  let marks =
    let s = Scad.color Color.Red @@ Scad.sphere 2. in
    List.map (fun { x; y } -> Scad.translate (v3 x y 0.) s) control_pts
  and line =
    let path = Path3.of_path2 CubicSpline.(interpolate_path (fit control_pts) 100) in
    Mesh.(to_scad @@ sweep ~transforms:(Path3.to_transforms ~euler:false path) square)
  in
  let scad = Scad.union (line :: marks)
  and oc = open_out "spline.scad" in
  Scad.write oc scad;
  close_out oc
