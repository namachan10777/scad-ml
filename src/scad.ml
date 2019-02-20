let pi = 4.0 *. atan 1.0

module Scad_core = struct
    type pos_t = float * float * float
    type rotate_t = float * float * float
    type scad_t =
        | Cylinder of { r1: float; r2: float; h: float; center: bool; fa: float option; fs: float option; fn: int option }
        | Cube of { size: (float * float * float); center: bool }
        | Sphere of { r: float; fa: float option; fs: float option; fn: int option }
        | Translate of pos_t * scad_t
        | Rotate of rotate_t * scad_t
        | Union of scad_t list
        | Intersection of scad_t list
        | Difference of scad_t * scad_t list
        | Polyhedron of pos_t list * int list list
        | Mirror of (int * int * int) * scad_t

    let string_of_pos_t = function (w, h, d) -> Printf.sprintf "[%f, %f, %f]" w h d
    let string_of_rotate_t =
        let deg_of_rad r = 360.0 *. r /. (2. *. pi) in
        function (w, h, d) -> Printf.sprintf "[%f, %f, %f]" (deg_of_rad w) (deg_of_rad h) (deg_of_rad d)

    let map f = function
        | Some(x) -> Some(f x)
        | None -> None

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

        in print ""
end

module Model = struct
    let cylinder ?(center=false) ?fa ?fs ?fn r h =
        Scad_core.Cylinder { r1=r; r2=r; h=h; center=false; fa; fs; fn }

    let cube ?(center=false) size =
        Scad_core.Cube { size=size; center=false }

    let sphere ?fa ?fs ?fn r = Scad_core.Sphere { r; fa; fs; fn }

    let translate p scad =
        Scad_core.Translate (p, scad)

    let rotate r scad =
        Scad_core.Rotate (r, scad)

    let union elements =
        Scad_core.Union elements

    let difference min sub =
        Scad_core.Difference (min, sub)

    let intersection elements =
        Scad_core.Intersection elements

    let polyhedron points faces =
        Scad_core.Polyhedron (points, faces)

    let mirror v scad =
        Scad_core.Mirror (v, scad)
end

module Scad = struct
    let write oc scad =
        Printf.fprintf oc "%s" (Scad_core.string_of_scad scad);
        flush oc
end

module Math = struct
    module Pos = struct
        let horizontal_op op p1 p2 = match p1, p2 with
            (x1, y1, z1), (x2, y2, z2) -> (op x1 x2), (op y1 y2), (op z1 z2)

        let add = horizontal_op (+.)
        let sub = horizontal_op (-.)
        let mul = horizontal_op ( *.)
        let div = horizontal_op (/.)
    end
end
