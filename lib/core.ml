let pi = 4.0 *. atan 1.0

type pos_t = float * float * float
type rotate_t = float * float * float
type scad_t =
    | Cylinder of { r1: float; r2: float; h: float; center: bool; fa: float option; fs: float option; fn: int option } | Cube of { size: (float * float * float); center: bool }
    | Sphere of { r: float; fa: float option; fs: float option; fn: int option }
    | Translate of pos_t * scad_t
    | Rotate of rotate_t * scad_t
    | Union of scad_t list | Intersection of scad_t list
    | Difference of scad_t * scad_t list
    | Minkowski of scad_t list
    | Hull of scad_t list
    | Polyhedron of pos_t list * int list list
    | Mirror of (int * int * int) * scad_t
    | Projection of { src: scad_t; cut: bool }
    | LinearExtrude of { src: scad_t; height: float option; center: bool; convexity: int; twist: int option; slices: int; scale: float; fn: int }

let string_of_pos_t = function (w, h, d) -> Printf.sprintf "[%f, %f, %f]" w h d
let string_of_rotate_t =
    let deg_of_rad r = 360.0 *. r /. (2. *. pi) in
    function (w, h, d) -> Printf.sprintf "[%f, %f, %f]" (deg_of_rad w) (deg_of_rad h) (deg_of_rad d)

let map f = function
    | Some(x) -> Some(f x)
    | None -> None

let either f x = function
    | Some(x) -> f x
    | None -> x

let rec join x = function
    | [] -> []
    | h :: [] -> h :: []
    | h :: t :: [] -> h :: x :: t :: []
    | h :: t -> h :: x :: join x t 

let rec compact = function
    | Some(h) :: t -> h :: compact t
    | None :: t -> compact t
    | [] -> []

let string_of_scad =
    let string_of_f_ fa fs fn =
        [map (fun fa -> Printf.sprintf "$fa=%f" fa) fa; 
         map (fun fs -> Printf.sprintf "$fs=%f" fs) fs; 
         map (fun fn -> Printf.sprintf "$fn=%d" fn) fn]
        |> compact
        |> join ", "
        |> begin function
            | [] -> ""
            | l -> List.fold_left (^) ", " l
        end
    in
    let rec arrange_elms indent = List.fold_left (fun stmts scad -> stmts^(print indent scad)) ""
    and print indent = function
        | Cylinder { r1; r2; h; center; fa; fs; fn } ->
                Printf.sprintf "%scylinder(h=%f, r1=%f, r2=%f, center=%B%s);\n" indent h r1 r2 center (string_of_f_ fa fs fn)
        | Cube { size; center } ->
            let (w, h, d) = size in
            Printf.sprintf "%scube(size=[%f, %f, %f], center=%B);\n" indent w h d center
        | Sphere { r; fa; fs; fn } ->
            Printf.sprintf "%ssphere(%f%s);\n" indent r (string_of_f_ fa fs fn)
        | Translate (p, scad) ->
            Printf.sprintf "%stranslate(%s)\n%s" indent (string_of_pos_t p) (print (indent^"\t") scad)
        | Rotate (r, scad) ->
            Printf.sprintf "%srotate(%s)\n%s" indent (string_of_rotate_t r) (print (indent^"\t") scad)
        | Union elements ->
            Printf.sprintf "%sunion(){\n%s%s}\n" indent (arrange_elms (indent^"\t") elements) indent
        | Intersection elements ->
            Printf.sprintf "%sintersection(){\n%s%s}\n" indent (arrange_elms (indent^"\t") elements) indent
        | Difference (minuend, subtrahend) ->
            Printf.sprintf "%sdifference(){\n%s%s%s}\n" indent (print (indent^"\t") minuend) (arrange_elms (indent^"\t") subtrahend) indent
        | Minkowski elements ->
            Printf.sprintf "%sminkowski(){\n%s%s}\n" indent (arrange_elms (indent^"\t") elements) indent
        | Hull elements ->
            Printf.sprintf "%shull(){\n%s%s}\n" indent (arrange_elms (indent^"\t") elements) indent
        | Polyhedron (points, faces) ->
            let string_of_list string_of l =
                "["^
                (List.fold_left (fun str elm -> str^", "^(string_of elm)) (l |> List.hd |> string_of) (List.tl l))^
                "]"
            in Printf.sprintf "%spolyhedron(points=%s, faces=%s);\n"
                indent
                (string_of_list string_of_pos_t points)
                (string_of_list (string_of_list string_of_int) faces)
        | Mirror ((x, y, z), scad) ->
            Printf.sprintf "%smirror(v=[%d, %d, %d])\n%s" indent x y z (print (indent^"\t") scad)
        | Projection { src; cut } ->
            Printf.sprintf "%sprojection(cut=%B){\n%s%s}\n" indent cut (print (indent ^ "\t") src) indent
        | LinearExtrude { src; height; center; convexity; twist; slices; scale; fn } ->
            Printf.sprintf "%slinear_extrude(%scenter=%B, convexity=%d, %sslices=%d, scale=%f, $fn=%d)\n%s"
            indent
            (either (Printf.sprintf "height=%f, ") "" height)
            center
            convexity
            (either (Printf.sprintf "twist=%d, ") "" twist)
            slices
            scale
            fn
            (print (indent^"\t") src)
    in print ""
