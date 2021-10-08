type two_d = TwoD
type three_d = ThreeD

type scad =
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
  | Color of
      { src : scad
      ; color : Color.t
      ; alpha : float option
      }
  | Translate of Vec3.t * scad
  | Rotate of Vec3.t * scad
  | VectorRotate of Vec3.t * float * scad
  | MultMatrix of MultMatrix.t * scad
  | Union of scad list
  | Intersection of scad list
  | Difference of scad * scad list
  | Minkowski of scad list
  | Hull of scad list
  | Polyhedron of
      { points : Vec3.t list
      ; faces : int list list
      ; convexity : int
      }
  | Mirror of (float * float * float) * scad
  | Projection of
      { src : scad
      ; cut : bool
      }
  | LinearExtrude of
      { src : scad
      ; height : float option
      ; center : bool
      ; convexity : int
      ; twist : int option
      ; slices : int
      ; scale : float * float
      ; fn : int
      }
  | RotateExtrude of
      { src : scad
      ; angle : float option
      ; convexity : int
      ; fa : float option
      ; fs : float option
      ; fn : int option
      }
  | Scale of (float * float * float) * scad
  | Resize of (float * float * float) * scad
  | Offset of
      { src : scad
      ; offset : [ `Radius of float | `Delta of float ]
      ; chamfer : bool
      }
  | Import of
      { file : string
      ; convexity : int
      ; dxf_layer : string option
      }

type 'space t =
  | D2 : scad -> two_d t
  | D3 : scad -> three_d t

let d2 scad = D2 scad
let d3 scad = D3 scad

let unpack : type a. a t -> scad = function
  | D2 scad -> scad
  | D3 scad -> scad

let map : type a. (scad -> scad) -> a t -> a t =
 fun f -> function
  | D2 scad -> D2 (f scad)
  | D3 scad -> D3 (f scad)

let cylinder ?(center = false) ?fa ?fs ?fn r h =
  d3 @@ Cylinder { r1 = r; r2 = r; h; center; fa; fs; fn }

let cone ?(center = false) ?fa ?fs ?fn ~height r1 r2 =
  d3 @@ Cylinder { r1; r2; h = height; center; fa; fs; fn }

let cube ?(center = false) size = d3 @@ Cube { size; center }
let sphere ?fa ?fs ?fn r = d3 @@ Sphere { r; fa; fs; fn }
let square ?(center = false) size = d2 @@ Square { size; center }
let circle ?fa ?fs ?fn r = d2 @@ Circle { r; fa; fs; fn }
let polygon ?(convexity = 10) ?paths points = d2 @@ Polygon { points; paths; convexity }

let text ?size ?font ?halign ?valign ?spacing ?direction ?language ?script ?fn str =
  d2
  @@ Text
       { text = str
       ; size
       ; font
       ; halign
       ; valign
       ; spacing
       ; direction
       ; language
       ; script
       ; fn
       }

let translate p = map (fun scad -> Translate (p, scad))
let rotate r = map (fun scad -> Rotate (r, scad))
let rotate_about_pt r p t = translate p t |> rotate r |> translate (Vec3.negate p)
let vector_rotate ax r = map (fun scad -> VectorRotate (ax, r, scad))

let vector_rotate_about_pt ax r p t =
  translate p t |> vector_rotate ax r |> translate (Vec3.negate p)

let multmatrix mat = map (fun scad -> MultMatrix (mat, scad))
let quaternion q = map (fun scad -> MultMatrix (Quaternion.to_multmatrix q, scad))
let quaternion_about_pt q p t = translate p t |> quaternion q |> translate (Vec3.negate p)
let union_2d ts = d2 @@ Union (List.map unpack ts)
let union_3d ts = d3 @@ Union (List.map unpack ts)

(* let nonempty
 *     (type a)
 *     ~(f2 : two_d t list -> two_d t)
 *     ~(f3 : three_d t list -> three_d t)
 *     (ts : a t list)
 *     : a t
 *   =
 *   match ts with
 *   | D2 _ :: _ -> f2 ts
 *   | D3 _ :: _ -> f3 ts
 *   | []        -> failwith "List must be non-empty." *)
(* let nonempty
 *     : type a.
 *       (two_d t list -> two_d t) -> (three_d t list -> three_d t) -> a t list -> a t
 *   =
 *  fun f2 f3 ts ->
 *   match ts with
 *   | D2 _ :: _ -> f2 ts
 *   | D3 _ :: _ -> f3 ts
 *   | []        -> failwith "List must be non-empty." *)

(* let union_nonempty = nonempty union_2d union_3d *)
let union_nonempty : type a. a t list -> a t =
 fun ts ->
  match ts with
  | D2 _ :: _ -> union_2d ts
  | D3 _ :: _ -> union_3d ts
  | []        -> failwith "List must be non-empty."

let minkowski_2d ts = d2 @@ Minkowski (List.map unpack ts)
let minkowski_3d ts = d3 @@ Minkowski (List.map unpack ts)

(* let minkowski_nonempty = nonempty minkowski_2d minkowski_3d *)
let minkowski_nonempty : type a. a t list -> a t =
 fun ts ->
  match ts with
  | D2 _ :: _ -> minkowski_2d ts
  | D3 _ :: _ -> minkowski_3d ts
  | []        -> failwith "List must be non-empty."

let hull_2d ts = d2 @@ Hull (List.map unpack ts)
let hull_3d ts = d3 @@ Hull (List.map unpack ts)

(* let hull_nonempty = nonempty hull_2d hull_3d *)
let hull_nonempty : type a. a t list -> a t =
 fun ts ->
  match ts with
  | D2 _ :: _ -> hull_2d ts
  | D3 _ :: _ -> hull_3d ts
  | []        -> failwith "List must be non-empty."

let difference (type a) (t : a t) (sub : a t list) =
  map (fun scad -> Difference (scad, List.map unpack sub)) t

let intersection_2d ts = d2 @@ Intersection (List.map unpack ts)
let intersection_3d ts = d3 @@ Intersection (List.map unpack ts)

(* let intersection_nonempty = nonempty intersection_2d intersection_3d *)
let intersection_nonempty : type a. a t list -> a t =
 fun ts ->
  match ts with
  | D2 _ :: _ -> intersection_2d ts
  | D3 _ :: _ -> intersection_3d ts
  | []        -> failwith "List must be non-empty."

let polyhedron ?(convexity = 10) points faces =
  d3 @@ Polyhedron { points; faces; convexity }

let mirror ax = map (fun scad -> Mirror (ax, scad))
let projection ?(cut = false) (D3 src) = d2 @@ Projection { src; cut }

let linear_extrude
    ?height
    ?(center = false)
    ?(convexity = 10)
    ?twist
    ?(slices = 20)
    ?(scale = 1.0, 1.0)
    ?(fn = 16)
    (D2 src)
  =
  d3 @@ LinearExtrude { src; height; center; convexity; twist; slices; scale; fn }

let rotate_extrude ?angle ?(convexity = 10) ?fa ?fs ?fn (D2 src) =
  d3 @@ RotateExtrude { src; angle; convexity; fa; fs; fn }

let scale factors = map (fun scad -> Scale (factors, scad))
let resize new_dims = map (fun scad -> Resize (new_dims, scad))
let offset ?(chamfer = false) offset (D2 src) = d2 @@ Offset { src; offset; chamfer }
let import ?dxf_layer ?(convexity = 10) file = Import { file; convexity; dxf_layer }
let import_2d ?dxf_layer ?convexity file = d2 (import ?dxf_layer ?convexity file)
let import_3d ?convexity file = d3 (import ?convexity file)
let color ?alpha color = map (fun src -> Color { src; color; alpha })
let ( |>> ) t p = translate p t
let ( |@> ) t r = rotate r t

let to_string t =
  let value_map f ~default = function
    | Some x -> f x
    | None   -> default
  and deg_of_rad r = 180.0 *. r /. Float.pi in
  let string_of_list f = function
    | h :: t ->
      List.fold_left
        (fun acc a -> Printf.sprintf "%s, %s" acc (f a))
        (Printf.sprintf "[%s" (f h))
        t
      ^ "]"
    | []     -> "[]"
  and maybe_fmt fmt opt = value_map (Printf.sprintf fmt) ~default:"" opt
  and string_of_f_ fa fs fn =
    [ Option.map (fun fa -> Printf.sprintf "$fa=%f" @@ deg_of_rad fa) fa
    ; Option.map (fun fs -> Printf.sprintf "$fs=%f" fs) fs
    ; Option.map (fun fn -> Printf.sprintf "$fn=%d" fn) fn
    ]
    |> List.filter_map Fun.id
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
        |> maybe_fmt ", paths=%s" )
        convexity
    | Text { text; size; font; halign; valign; spacing; direction; language; script; fn }
      ->
      Printf.sprintf
        "%stext(\"%s\"%s%s%s%s%s%s%s%s%s);\n"
        indent
        text
        (maybe_fmt ", size=\"%f\"" size)
        (maybe_fmt ", font=\"%s\"" font)
        (Option.map Text.h_align_to_string halign |> maybe_fmt ", halign=\"%s\"")
        (Option.map Text.v_align_to_string valign |> maybe_fmt ", valign=\"%s\"")
        (maybe_fmt ", spacing=\"%f\"" spacing)
        (Option.map Text.direction_to_string direction |> maybe_fmt ", direction=\"%s\"")
        (maybe_fmt ", language=\"%s\"" language)
        (maybe_fmt ", script=\"%s\"" script)
        (maybe_fmt ", $fn=\"%i\"" fn)
    | Translate (p, scad) ->
      Printf.sprintf
        "%stranslate(%s)\n%s"
        indent
        (Vec3.to_string p)
        (print (indent ^ "\t") scad)
    | Rotate (r, scad) ->
      Printf.sprintf
        "%srotate(%s)\n%s"
        indent
        (Vec3.deg_of_rad r |> Vec3.to_string)
        (print (indent ^ "\t") scad)
    | VectorRotate (axis, r, scad) ->
      Printf.sprintf
        "%srotate(a=%f, v=%s)\n%s"
        indent
        (deg_of_rad r)
        (Vec3.to_string axis)
        (print (indent ^ "\t") scad)
    | MultMatrix (mat, scad) ->
      Printf.sprintf
        "%smultmatrix(%s)\n%s"
        indent
        (MultMatrix.to_string mat)
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
    | Polyhedron { points; faces; convexity } ->
      Printf.sprintf
        "%spolyhedron(points=%s, faces=%s, convexity=%i);\n"
        indent
        (string_of_list Vec3.to_string points)
        (string_of_list (string_of_list string_of_int) faces)
        convexity
    | Mirror ((x, y, z), scad) ->
      Printf.sprintf
        "%smirror(v=[%f, %f, %f])\n%s"
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
    | LinearExtrude { src; height; center; convexity; twist; slices; scale = sx, sy; fn }
      ->
      Printf.sprintf
        "%slinear_extrude(%scenter=%B, convexity=%d, %sslices=%d, scale=[%f, %f], $fn=%d)\n\
         %s"
        indent
        (maybe_fmt "height=%f, " height)
        center
        convexity
        (maybe_fmt "twist=%d, " twist)
        slices
        sx
        sy
        fn
        (print (indent ^ "\t") src)
    | RotateExtrude { src; angle; convexity; fa; fs; fn } ->
      Printf.sprintf
        "%srotate_extrude(%sconvexity=%d%s)\n%s"
        indent
        (Option.map deg_of_rad angle |> maybe_fmt "angle=%f")
        convexity
        (string_of_f_ fa fs fn)
        (print (indent ^ "\t") src)
    | Scale (p, scad) ->
      Printf.sprintf
        "%sscale(%s)\n%s"
        indent
        (Vec3.to_string p)
        (print (indent ^ "\t") scad)
    | Resize (p, scad) ->
      Printf.sprintf
        "%sresize(%s)\n%s"
        indent
        (Vec3.to_string p)
        (print (indent ^ "\t") scad)
    | Offset { src; offset; chamfer } ->
      Printf.sprintf
        "%soffset(%s, chamfer=%B)\n%s"
        indent
        ( match offset with
        | `Radius r -> Printf.sprintf "r = %f" r
        | `Delta d  -> Printf.sprintf "delta = %f" d )
        chamfer
        (print (indent ^ "\t") src)
    | Import { file; convexity; dxf_layer } ->
      Printf.sprintf
        "%simport(\"%s\", convexity=%i%s);\n"
        indent
        file
        convexity
        (maybe_fmt ", layer=%s" dxf_layer)
    | Color { src; color; alpha } ->
      Printf.sprintf
        "%scolor(%s%s)\n%s"
        indent
        (Color.to_string color)
        (maybe_fmt ", alpha=%f" alpha)
        (print (indent ^ "\t") src)
  in
  print "" (unpack t)

let write oc t =
  Printf.fprintf oc "%s" (to_string t);
  flush oc
