open Scad_ml

let bezier_path () =
  let control_pts = Vec2.[ v 0. 10.; v 10. 40.; v 20. 40.; v 30. (-20.); v 40. (-40.) ]
  and square = Poly2.square ~center:true (v2 2. 2.) in
  let marks =
    let s = Scad.color Color.Red @@ Scad.sphere 2. in
    List.map (fun { x; y } -> Scad.translate (v3 x y 0.) s) control_pts
  and line =
    let path = List.map Vec2.to_vec3 @@ Bezier2.(curve ~fn:100 (of_path control_pts)) in
    Mesh.(to_scad @@ sweep ~transforms:(Path3.to_transforms ~euler:false path) square)
  in
  let scad = Scad.union (line :: marks)
  and oc = open_out "bezier_path.scad" in
  Scad.write oc scad;
  close_out oc
