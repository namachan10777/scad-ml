open Scad_ml

let () =
  let scad =
    let f ~z ~a:_ = Float.(5. *. (cos (log ((z /. 5.) +. 1.) *. pi) +. 2.)) in
    let outer = Mesh.axial_plot ~min_z:0. ~z_steps:50 ~max_z:50. f
    and inner =
      Mesh.axial_plot ~min_z:2. ~z_steps:50 ~max_z:51. (fun ~z ~a -> f ~z ~a -. 2.)
    in
    Scad.difference (Mesh.to_scad outer) [ Mesh.to_scad inner ]
  in
  Scad.to_file "axial_chalice.scad" scad
