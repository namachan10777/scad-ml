let pi = Core.pi
type t = (float * float * float)
let horizontal_op op p1 p2 = match p1, p2 with
    (x1, y1, z1), (x2, y2, z2) -> (op x1 x2), (op y1 y2), (op z1 z2)

let add = horizontal_op (+.)
let sub = horizontal_op (-.)
let mul = horizontal_op ( *.)
let div = horizontal_op (/.)
