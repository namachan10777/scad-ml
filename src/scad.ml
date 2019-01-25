type pos_t = float * float * float
type rotate_t = float * float * float
type scad_t =
    | Cylinder of { r1: float; r2: float; h: float; center: bool }
    | Cube of { size: (float * float * float); center: bool }
    | Sphere of { r: float }
    | Translate of pos_t * scad_t
    | Rotate of rotate_t * scad_t
    | Union of scad_t list
    | Intersection of scad_t list
    | Difference of scad_t list * scad_t

let string_of_scad = function
    | Cylinder { r1; r2; h; center } ->
        Printf.sprintf "cylinder(h=%f, r1=%f, r2=%f, center=%B)" h r1 r2 center
    | Cube { size; center } ->
        let (w, h, d) = size in
        Printf.sprintf "cube(size=[%f, %f, %f], center=%B)" w h d center
    | Sphere { r } ->
        Printf.sprintf "sphere(%f)" r
    | _ -> ""
