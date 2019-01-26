let pi = 4.0 *. atan 1.0

module Scad_core = struct
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
        | Difference of scad_t * scad_t list

    let string_of_pos_t = function (w, h, d) -> Printf.sprintf "[%f, %f, %f]" w h d
    let string_of_rotate_t =
        let deg_of_rad r = 360.0 *. r /. (2. *. pi) in
        function (w, h, d) -> Printf.sprintf "[%f, %f, %f]" (deg_of_rad w) (deg_of_rad h) (deg_of_rad d)

    let string_of_scad =
        let rec arrange_elms indent = List.fold_left (fun stmts scad -> stmts^(print indent scad)) ""
        and print indent = function
            | Cylinder { r1; r2; h; center } ->
                    Printf.sprintf "%scylinder(h=%f, r1=%f, r2=%f, center=%B);\n" indent h r1 r2 center
            | Cube { size; center } ->
                let (w, h, d) = size in
                Printf.sprintf "%scube(size=[%f, %f, %f], center=%B);\n" indent w h d center
            | Sphere { r } ->
                Printf.sprintf "%ssphere(%f);\n" indent r
            | Translate (p, scad) ->
                Printf.sprintf "%stranslate(%s);\n%s" indent (string_of_pos_t p) (print (indent^"\t") scad)
            | Rotate (r, scad) ->
                Printf.sprintf "%srotate(%s);\n%s" indent (string_of_rotate_t r) (print (indent^"\t") scad)
            | Union elements ->
                Printf.sprintf "%sunion(){\n%s}\n" indent @@ arrange_elms (indent^"\t") elements
            | Intersection elements ->
                Printf.sprintf "%sintersection(){\n%s}\n" indent @@ arrange_elms (indent^"\t") elements
            | Difference (minuend, subtrahend) ->
                Printf.sprintf "%sdifference(){\n%s%s}\n" indent (print (indent^"\t") minuend) @@ arrange_elms (indent^"\t") subtrahend
        in print ""
end

module Model = struct
    let cylinder ?(center=false) r h =
        Scad_core.Cylinder { r1=r; r2=r; h=h; center=false }

    let cube ?(center=false) size =
        Scad_core.Cube { size=size; center=false }

    let sphere r = Scad_core.Sphere { r }

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
end

module Scad = struct
    let write oc scad = Printf.fprintf oc "%s" (Scad_core.string_of_scad scad)
end
