open Scad_ml

let () =
  let scad =
    let gravity_well ~x ~y =
      let z = 50. -. (50. /. Float.sqrt ((x *. x) +. (y *. y))) in
      if z < 1. then 1. else z
    in
    Mesh.cartesian_plot
      ~min_x:(-10.)
      ~x_steps:30
      ~max_x:0.
      ~min_y:(-10.)
      ~y_steps:60
      ~max_y:10.
      gravity_well
    |> Mesh.to_scad
  in
  Scad.to_file "cartesian_gravity_well.scad" scad
