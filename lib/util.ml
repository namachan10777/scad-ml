let get_x (x, _, _) = x
let get_y (_, y, _) = y
let get_z (_, _, z) = z
let ( |>> ) src p = Model.translate p src
let ( |@> ) src r = Model.rotate r src
let ( <+> ) = Math.add
let ( <-> ) = Math.sub
let ( <*> ) = Math.mul
let ( </> ) = Math.div
let rotate_about_pt r p scad = scad |>> p |@> r |>> Math.negate p

let write oc scad =
  Printf.fprintf oc "%s" (Core.string_of_scad scad);
  flush oc
