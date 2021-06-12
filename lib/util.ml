let get_x (x, _, _) = x
let get_y (_, y, _) = y
let get_z (_, _, z) = z
let ( |>> ) src p = Model.translate p src
let ( |@> ) src r = Model.rotate r src
let ( <+> ) = Math.add
let ( <-> ) = Math.sub
let ( <*> ) = Math.mul
let ( </> ) = Math.div
let string_of_point (x, y, z) = Printf.sprintf "(%.3f, %.3f, %.3f)" x y z
let mat_of_list = Core.MultMatrix.of_list
let mat_of_list_exn = Core.MultMatrix.of_list_exn

let write oc scad =
  Printf.fprintf oc "%s" (Core.string_of_scad scad);
  flush oc
