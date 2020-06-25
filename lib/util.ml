let get_x = function (x, _, _) -> x
let get_y = function (_, y, _) -> y
let get_z = function (_, _, z) -> z

let ( |>> ) src p = Model.translate p src
let ( |@> ) src r = Model.rotate r src
let ( <+> ) = Math.add
let ( <-> ) = Math.sub
let ( <*> ) = Math.mul
let ( </> ) = Math.div

let write oc scad =
    Printf.fprintf oc "%s" (Core.string_of_scad scad);
    flush oc
