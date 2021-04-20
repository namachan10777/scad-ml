let pi = 4.0 *. atan 1.0

type pos_t = float * float * float
type rotate_t = float * float * float

module Text = struct
  type h_align =
    | Left
    | Center
    | Right

  let h_align_to_string = function
    | Left   -> "left"
    | Center -> "center"
    | Right  -> "right"

  type v_align =
    | Top
    | Center
    | Baseline

  let v_align_to_string = function
    | Top      -> "top"
    | Center   -> "center"
    | Baseline -> "baseline"

  type direction =
    | LeftToRight
    | RightToLeft
    | TopToBottom
    | BottomToTop

  let direction_to_string = function
    | LeftToRight -> "ltr"
    | RightToLeft -> "rtl"
    | TopToBottom -> "ttb"
    | BottomToTop -> "btt"

  type t =
    { text : string
    ; size : float option
    ; font : string option
    ; halign : h_align option
    ; valign : v_align option
    ; spacing : float option
    ; direction : direction option
    ; language : string option
    ; script : string option
    ; fn : int option
    }
end

type scad_t =
  | Cylinder of
      { r1 : float
      ; r2 : float
      ; h : float
      ; center : bool
      ; fa : float option
      ; fs : float option
      ; fn : int option
      }
  | Cube of
      { size : float * float * float
      ; center : bool
      }
  | Sphere of
      { r : float
      ; fa : float option
      ; fs : float option
      ; fn : int option
      }
  | Square of
      { size : float * float
      ; center : bool
      }
  | Circle of
      { r : float
      ; fa : float option
      ; fs : float option
      ; fn : int option
      }
  | Polygon of
      { points : (float * float) list
      ; paths : int list list option
      ; convexity : int
      }
  | Text of Text.t
  | Translate of pos_t * scad_t
  | Rotate of rotate_t * scad_t
  | Union of scad_t list
  | Intersection of scad_t list
  | Difference of scad_t * scad_t list
  | Minkowski of scad_t list
  | Hull of scad_t list
  | Polyhedron of pos_t list * int list list
  | Mirror of (int * int * int) * scad_t
  | Projection of
      { src : scad_t
      ; cut : bool
      }
  | LinearExtrude of
      { src : scad_t
      ; height : float option
      ; center : bool
      ; convexity : int
      ; twist : int option
      ; slices : int
      ; scale : float
      ; fn : int
      }
  | RotateExtrude of
      { src : scad_t
      ; angle : float option
      ; convexity : int
      ; fa : float option
      ; fs : float option
      ; fn : int option
      }

let string_of_pos_t = function
  | w, h, d -> Printf.sprintf "[%f, %f, %f]" w h d

let deg_of_rad r = 360.0 *. r /. (2. *. pi)

let string_of_rotate_t = function
  | w, h, d -> Printf.sprintf "[%f, %f, %f]" (deg_of_rad w) (deg_of_rad h) (deg_of_rad d)

let string_of_list f = function
  | h :: t ->
    List.fold_left
      (fun acc a -> Printf.sprintf "%s, %s" acc (f a))
      (Printf.sprintf "[%s" (f h))
      t
    ^ "]"
  | []     -> "[]"

let value_map f ~default = function
  | Some x -> f x
  | None   -> default

let maybe_param fmt param_opt = value_map (Printf.sprintf fmt) ~default:"" param_opt

let rec join x = function
  | []       -> []
  | [ h ]    -> [ h ]
  | [ h; t ] -> [ h; x; t ]
  | h :: t   -> h :: x :: join x t

let rec compact = function
  | Some h :: t -> h :: compact t
  | None :: t   -> compact t
  | []          -> []

let string_of_scad =
  let string_of_f_ fa fs fn =
    [ Option.map (fun fa -> Printf.sprintf "$fa=%f" fa) fa
    ; Option.map (fun fs -> Printf.sprintf "$fs=%f" fs) fs
    ; Option.map (fun fn -> Printf.sprintf "$fn=%d" fn) fn
    ]
    |> compact
    |> join ", "
    |> function
    | [] -> ""
    | l  -> List.fold_left ( ^ ) ", " l
  in
  let rec arrange_elms indent =
    List.fold_left (fun stmts scad -> stmts ^ print indent scad) ""
  and print indent = function
    | Cylinder { r1; r2; h; center; fa; fs; fn } ->
      Printf.sprintf
        "%scylinder(h=%f, r1=%f, r2=%f, center=%B%s);\n"
        indent
        h
        r1
        r2
        center
        (string_of_f_ fa fs fn)
    | Cube { size = w, h, d; center } ->
      Printf.sprintf "%scube(size=[%f, %f, %f], center=%B);\n" indent w h d center
    | Sphere { r; fa; fs; fn } ->
      Printf.sprintf "%ssphere(%f%s);\n" indent r (string_of_f_ fa fs fn)
    | Square { size = w, h; center } ->
      Printf.sprintf "%ssquare(size=[%f, %f], center=%B);\n" indent w h center
    | Circle { r; fa; fs; fn } ->
      Printf.sprintf "%scircle(%f%s);\n" indent r (string_of_f_ fa fs fn)
    | Polygon { points; paths; convexity } ->
      Printf.sprintf
        "%spolygon(points=%s%s, convexity=%d);\n"
        indent
        (string_of_list (fun (w, h) -> Printf.sprintf "[%f, %f]" w h) points)
        ( Option.map (string_of_list (string_of_list string_of_int)) paths
        |> maybe_param ", paths=%s" )
        convexity
    | Text { text; size; font; halign; valign; spacing; direction; language; script; fn }
      ->
      Printf.sprintf
        "%stext(\"%s\"%s%s%s%s%s%s%s%s%s);\n"
        indent
        text
        (maybe_param ", size=\"%f\"" size)
        (maybe_param ", font=\"%s\"" font)
        (Option.map Text.h_align_to_string halign |> maybe_param ", halign=\"%s\"")
        (Option.map Text.v_align_to_string valign |> maybe_param ", valign=\"%s\"")
        (maybe_param ", spacing=\"%f\"" spacing)
        (Option.map Text.direction_to_string direction |> maybe_param ", direction=\"%s\"")
        (maybe_param ", language=\"%s\"" language)
        (maybe_param ", script=\"%s\"" script)
        (maybe_param ", $fn=\"%i\"" fn)
    | Translate (p, scad) ->
      Printf.sprintf
        "%stranslate(%s)\n%s"
        indent
        (string_of_pos_t p)
        (print (indent ^ "\t") scad)
    | Rotate (r, scad) ->
      Printf.sprintf
        "%srotate(%s)\n%s"
        indent
        (string_of_rotate_t r)
        (print (indent ^ "\t") scad)
    | Union elements ->
      Printf.sprintf
        "%sunion(){\n%s%s}\n"
        indent
        (arrange_elms (indent ^ "\t") elements)
        indent
    | Intersection elements ->
      Printf.sprintf
        "%sintersection(){\n%s%s}\n"
        indent
        (arrange_elms (indent ^ "\t") elements)
        indent
    | Difference (minuend, subtrahend) ->
      Printf.sprintf
        "%sdifference(){\n%s%s%s}\n"
        indent
        (print (indent ^ "\t") minuend)
        (arrange_elms (indent ^ "\t") subtrahend)
        indent
    | Minkowski elements ->
      Printf.sprintf
        "%sminkowski(){\n%s%s}\n"
        indent
        (arrange_elms (indent ^ "\t") elements)
        indent
    | Hull elements ->
      Printf.sprintf
        "%shull(){\n%s%s}\n"
        indent
        (arrange_elms (indent ^ "\t") elements)
        indent
    | Polyhedron (points, faces) ->
      Printf.sprintf
        "%spolyhedron(points=%s, faces=%s);\n"
        indent
        (string_of_list string_of_pos_t points)
        (string_of_list (string_of_list string_of_int) faces)
    | Mirror ((x, y, z), scad) ->
      Printf.sprintf
        "%smirror(v=[%d, %d, %d])\n%s"
        indent
        x
        y
        z
        (print (indent ^ "\t") scad)
    | Projection { src; cut } ->
      Printf.sprintf
        "%sprojection(cut=%B){\n%s%s}\n"
        indent
        cut
        (print (indent ^ "\t") src)
        indent
    | LinearExtrude { src; height; center; convexity; twist; slices; scale; fn } ->
      Printf.sprintf
        "%slinear_extrude(%scenter=%B, convexity=%d, %sslices=%d, scale=%f, $fn=%d)\n%s"
        indent
        (maybe_param "height=%f, " height)
        center
        convexity
        (maybe_param "twist=%d, " twist)
        slices
        scale
        fn
        (print (indent ^ "\t") src)
    | RotateExtrude { src; angle; convexity; fa; fs; fn } ->
      Printf.sprintf
        "%srotate_extrude(%sconvexity=%d%s)\n%s"
        indent
        (Option.map deg_of_rad angle |> maybe_param "angle=%f")
        convexity
        (string_of_f_ fa fs fn)
        (print (indent ^ "\t") src)
  in
  print ""
