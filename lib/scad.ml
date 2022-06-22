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
      { size : Vec3.t
      ; center : bool
      }
  | Sphere of
      { r : float
      ; fa : float option
      ; fs : float option
      ; fn : int option
      }
  | Square of
      { size : Vec2.t
      ; center : bool
      }
  | Circle of
      { r : float
      ; fa : float option
      ; fs : float option
      ; fn : int option
      }
  | Polygon of
      { points : Vec2.t list
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
  | Mirror of Vec3.t * scad
  | Projection of
      { src : scad
      ; cut : bool
      }
  | LinearExtrude of
      { src : scad
      ; height : float
      ; center : bool
      ; convexity : int
      ; twist : int option
      ; slices : int
      ; scale : Vec2.t
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
  | Scale of Vec3.t * scad
  | Resize of Vec3.t * scad
  | Offset of
      { src : scad
      ; offset : [ `Radius of float | `Delta of float | `Chamfer of float ]
      }
  | Import of
      { file : string
      ; convexity : int
      ; dxf_layer : string option
      }
  | Surface of
      { file : string
      ; center : bool
      ; invert : bool
      ; convexity : int
      }
  | Render of
      { src : scad
      ; convexity : int
      }

type ('space, 'rot) t =
  | D2 : scad -> (Vec2.t, float) t
  | D3 : scad -> (Vec3.t, Vec3.t) t

type d2 = (Vec2.t, float) t
type d3 = (Vec3.t, Vec3.t) t

let d2 scad = D2 scad
let d3 scad = D3 scad

let unpack : type a b. (a, b) t -> scad = function
  | D2 scad -> scad
  | D3 scad -> scad

let map : type a b. (scad -> scad) -> (a, b) t -> (a, b) t =
 fun f -> function
  | D2 scad -> D2 (f scad)
  | D3 scad -> D3 (f scad)

let cylinder ?(center = false) ?fa ?fs ?fn ~height:h r =
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

let translate (type a b) (p : a) : (a, b) t -> (a, b) t = function
  | D2 scad -> d2 @@ Translate (Vec3.of_vec2 p, scad)
  | D3 scad -> d3 @@ Translate (p, scad)

let rotate (type a b) (r : b) : (a, b) t -> (a, b) t = function
  | D2 scad -> d2 @@ Rotate ({ x = 0.; y = 0.; z = r }, scad)
  | D3 scad -> d3 @@ Rotate (r, scad)

let rotate_about_pt (type a b) (r : b) (p : a) (t : (a, b) t) : (a, b) t =
  let p' : a =
    match t with
    | D2 _ -> Vec2.negate p
    | D3 _ -> Vec3.negate p
  in
  translate p' t |> rotate r |> translate p

let vector_rotate ax r (D3 scad) = d3 @@ VectorRotate (ax, r, scad)

let vector_rotate_about_pt ax r p t =
  translate (Vec3.negate p) t |> vector_rotate ax r |> translate p

let multmatrix mat (D3 scad) = d3 @@ MultMatrix (mat, scad)
let quaternion q (D3 scad) = d3 @@ MultMatrix (Quaternion.to_multmatrix q, scad)
let quaternion_about_pt q p t = translate (Vec3.negate p) t |> quaternion q |> translate p
let union_2d ts = d2 @@ Union (List.map unpack ts)
let union_3d ts = d3 @@ Union (List.map unpack ts)

let empty_exn n =
  invalid_arg
    (Printf.sprintf
       "List must be non-empty. Use %s_2d or %s_3d if empty lists are expected."
       n
       n )

let union : type a b. (a, b) t list -> (a, b) t =
 fun ts ->
  match ts with
  | D2 _ :: _ -> union_2d ts
  | D3 _ :: _ -> union_3d ts
  | []        -> empty_exn "union"

let minkowski_2d ts = d2 @@ Minkowski (List.map unpack ts)
let minkowski_3d ts = d3 @@ Minkowski (List.map unpack ts)

let minkowski : type a b. (a, b) t list -> (a, b) t =
 fun ts ->
  match ts with
  | D2 _ :: _ -> minkowski_2d ts
  | D3 _ :: _ -> minkowski_3d ts
  | []        -> empty_exn "minkowski"

let hull_2d ts = d2 @@ Hull (List.map unpack ts)
let hull_3d ts = d3 @@ Hull (List.map unpack ts)

let hull : type a b. (a, b) t list -> (a, b) t =
 fun ts ->
  match ts with
  | D2 _ :: _ -> hull_2d ts
  | D3 _ :: _ -> hull_3d ts
  | []        -> empty_exn "hull"

let difference (type a b) (t : (a, b) t) (sub : (a, b) t list) =
  map (fun scad -> Difference (scad, List.map unpack sub)) t

let intersection_2d ts = d2 @@ Intersection (List.map unpack ts)
let intersection_3d ts = d3 @@ Intersection (List.map unpack ts)

let intersection : type a b. (a, b) t list -> (a, b) t =
 fun ts ->
  match ts with
  | D2 _ :: _ -> intersection_2d ts
  | D3 _ :: _ -> intersection_3d ts
  | []        -> empty_exn "intersection"

let polyhedron ?(convexity = 10) points faces =
  d3 @@ Polyhedron { points; faces; convexity }

let mirror (type a b) (ax : a) : (a, b) t -> (a, b) t = function
  | D2 scad -> d2 @@ Mirror (Vec3.of_vec2 ax, scad)
  | D3 scad -> d3 @@ Mirror (ax, scad)

let projection ?(cut = false) (D3 src) = d2 @@ Projection { src; cut }

let linear_extrude
    ?(height = 10.)
    ?(center = false)
    ?(convexity = 10)
    ?twist
    ?(slices = 20)
    ?(scale = Vec2.v 1. 1.)
    ?(fn = 16)
    (D2 src)
  =
  if height <= 0. then invalid_arg "Extrusion height must be positive.";
  d3 @@ LinearExtrude { src; height; center; convexity; twist; slices; scale; fn }

let rotate_extrude ?angle ?(convexity = 10) ?fa ?fs ?fn (D2 src) =
  d3 @@ RotateExtrude { src; angle; convexity; fa; fs; fn }

let scale (type a b) (factors : a) : (a, b) t -> (a, b) t = function
  | D2 scad -> d2 @@ Scale (Vec3.of_vec2 factors, scad)
  | D3 scad -> d3 @@ Scale (factors, scad)

let resize (type a b) (new_dims : a) : (a, b) t -> (a, b) t = function
  | D2 scad -> d2 @@ Resize (Vec3.of_vec2 new_dims, scad)
  | D3 scad -> d3 @@ Resize (new_dims, scad)

let offset offset (D2 src) = d2 @@ Offset { src; offset }
let import ?dxf_layer ?(convexity = 10) file = Import { file; convexity; dxf_layer }
let d2_import_exts = Export.ExtSet.of_list [ ".dxf"; ".svg" ]
let d3_import_exts = Export.ExtSet.of_list [ ".stl"; ".off"; ".amf"; ".3mf" ]

let import_2d ?dxf_layer ?convexity file =
  match Export.legal_ext d2_import_exts file with
  | Ok ()     -> d2 @@ import ?dxf_layer ?convexity file
  | Error ext ->
    invalid_arg
      (Printf.sprintf "Input file extension %s is not supported for 2D import." ext)

let import_3d ?convexity file =
  match Export.legal_ext d3_import_exts file with
  | Ok ()     -> d3 @@ import ?convexity file
  | Error ext ->
    invalid_arg
      (Printf.sprintf "Input file extension %s is not supported for 3D import." ext)

let surface ?(convexity = 10) ?(center = false) ?(invert = false) file =
  match Filename.extension file with
  | ".dat" | ".png" -> d3 @@ Surface { file; center; invert; convexity }
  | ext             ->
    invalid_arg
    @@ (Printf.sprintf "Input file extension %s is not supported for surface import.") ext

let color ?alpha color = map (fun src -> Color { src; color; alpha })
let render ?(convexity = 10) = map (fun src -> Render { src; convexity })

let to_string t =
  let buf_add_list b f = function
    | h :: t ->
      let append a =
        Buffer.add_char b ',';
        f b a
      in
      Buffer.add_char b '[';
      f b h;
      List.iter append t;
      Buffer.add_char b ']'
    | []     ->
      let b = Buffer.create 2 in
      Buffer.add_char b '[';
      Buffer.add_char b ']'
  in
  let buf_of_list f l =
    let b = Buffer.create 512 in
    buf_add_list b f l;
    b
  and buf_add_idxs b = buf_add_list b (fun b' i -> Buffer.add_string b' (Int.to_string i))
  and buf_add_vec2 b Vec.{ x; y } =
    Buffer.add_char b '[';
    Buffer.add_string b (Float.to_string x);
    Buffer.add_char b ',';
    Buffer.add_string b (Float.to_string y);
    Buffer.add_char b ']'
  and buf_add_vec3 b Vec.{ x; y; z } =
    Buffer.add_char b '[';
    Buffer.add_string b (Float.to_string x);
    Buffer.add_char b ',';
    Buffer.add_string b (Float.to_string y);
    Buffer.add_char b ',';
    Buffer.add_string b (Float.to_string z);
    Buffer.add_char b ']'
  and maybe_fmt fmt opt = Util.value_map_opt (Printf.sprintf fmt) ~default:"" opt
  and string_of_f_ fa fs (fn : int option) =
    let fa_to_string a = Float.to_string @@ Math.deg_of_rad a in
    Printf.sprintf
      ", $fa=%s, $fs=%s, $fn=%i"
      (Util.value_map_opt ~default:"12" fa_to_string fa)
      (Util.value_map_opt ~default:"2" Float.to_string fs)
      (Option.value ~default:0 fn)
  in
  let rec arrange_elms indent scads =
    let buf = Buffer.create 100 in
    List.iter (fun scad -> Buffer.add_string buf (print indent scad)) scads;
    Buffer.contents buf
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
    | Cube { size = { x; y; z }; center } ->
      Printf.sprintf "%scube(size=[%f, %f, %f], center=%B);\n" indent x y z center
    | Sphere { r; fa; fs; fn } ->
      Printf.sprintf "%ssphere(%f%s);\n" indent r (string_of_f_ fa fs fn)
    | Square { size = { x; y }; center } ->
      Printf.sprintf "%ssquare(size=[%f, %f], center=%B);\n" indent x y center
    | Circle { r; fa; fs; fn } ->
      Printf.sprintf "%scircle(%f%s);\n" indent r (string_of_f_ fa fs fn)
    | Polygon { points; paths; convexity } ->
      Printf.sprintf
        "%spolygon(points=%s%s, convexity=%d);\n"
        indent
        (Buffer.contents @@ buf_of_list buf_add_vec2 points)
        ( Option.map (fun ps -> Buffer.contents @@ buf_of_list buf_add_idxs ps) paths
        |> maybe_fmt ", paths=%s" )
        convexity
    | Text { text; size; font; halign; valign; spacing; direction; language; script; fn }
      ->
      Printf.sprintf
        "%stext(\"%s\"%s%s%s%s%s%s%s%s%s);\n"
        indent
        text
        (maybe_fmt ", size=%f" size)
        (maybe_fmt ", font=\"%s\"" font)
        (maybe_fmt ", halign=\"%s\"" @@ Option.map Text.h_align_to_string halign)
        (maybe_fmt ", valign=\"%s\"" @@ Option.map Text.v_align_to_string valign)
        (maybe_fmt ", spacing=%f" spacing)
        (maybe_fmt ", direction=\"%s\"" @@ Option.map Text.direction_to_string direction)
        (maybe_fmt ", language=\"%s\"" language)
        (maybe_fmt ", script=\"%s\"" script)
        (maybe_fmt ", $fn=\"%i\"" fn)
    | Translate (p, scad) ->
      Printf.sprintf
        "%stranslate(%s)\n%s"
        indent
        (Vec3.to_string p)
        (print (Printf.sprintf "%s\t" indent) scad)
    | Rotate (r, scad) ->
      Printf.sprintf
        "%srotate(%s)\n%s"
        indent
        (Vec3.to_string @@ Vec3.deg_of_rad r)
        (print (Printf.sprintf "%s\t" indent) scad)
    | VectorRotate (axis, r, scad) ->
      Printf.sprintf
        "%srotate(a=%f, v=%s)\n%s"
        indent
        (Math.deg_of_rad r)
        (Vec3.to_string axis)
        (print (Printf.sprintf "%s\t" indent) scad)
    | MultMatrix (mat, scad) ->
      Printf.sprintf
        "%smultmatrix(%s)\n%s"
        indent
        (MultMatrix.to_string mat)
        (print (Printf.sprintf "%s\t" indent) scad)
    | Union elements ->
      Printf.sprintf
        "%sunion(){\n%s%s}\n"
        indent
        (arrange_elms (Printf.sprintf "%s\t" indent) elements)
        indent
    | Intersection elements ->
      Printf.sprintf
        "%sintersection(){\n%s%s}\n"
        indent
        (arrange_elms (Printf.sprintf "%s\t" indent) elements)
        indent
    | Difference (minuend, subtrahend) ->
      Printf.sprintf
        "%sdifference(){\n%s%s%s}\n"
        indent
        (print (Printf.sprintf "%s\t" indent) minuend)
        (arrange_elms (Printf.sprintf "%s\t" indent) subtrahend)
        indent
    | Minkowski elements ->
      Printf.sprintf
        "%sminkowski(){\n%s%s}\n"
        indent
        (arrange_elms (Printf.sprintf "%s\t" indent) elements)
        indent
    | Hull elements ->
      Printf.sprintf
        "%shull(){\n%s%s}\n"
        indent
        (arrange_elms (Printf.sprintf "%s\t" indent) elements)
        indent
    | Polyhedron { points; faces; convexity } ->
      Printf.sprintf
        "%spolyhedron(points=%s, faces=%s, convexity=%i);\n"
        indent
        (Buffer.contents @@ buf_of_list buf_add_vec3 points)
        (Buffer.contents @@ buf_of_list buf_add_idxs faces)
        convexity
    | Mirror ({ x; y; z }, scad) ->
      Printf.sprintf
        "%smirror(v=[%f, %f, %f])\n%s"
        indent
        x
        y
        z
        (print (Printf.sprintf "%s\t" indent) scad)
    | Projection { src; cut } ->
      Printf.sprintf
        "%sprojection(cut=%B){\n%s%s}\n"
        indent
        cut
        (print (Printf.sprintf "%s\t" indent) src)
        indent
    | LinearExtrude
        { src; height; center; convexity; twist; slices; scale = { x; y }; fn } ->
      Printf.sprintf
        "%slinear_extrude(height=%f, center=%B, convexity=%d, %sslices=%d, scale=[%f, \
         %f], $fn=%d)\n\
         %s"
        indent
        height
        center
        convexity
        (maybe_fmt "twist=%d, " twist)
        slices
        x
        y
        fn
        (print (Printf.sprintf "%s\t" indent) src)
    | RotateExtrude { src; angle; convexity; fa; fs; fn } ->
      Printf.sprintf
        "%srotate_extrude(%sconvexity=%d%s)\n%s"
        indent
        (maybe_fmt "angle=%f" @@ Option.map Math.deg_of_rad angle)
        convexity
        (string_of_f_ fa fs fn)
        (print (Printf.sprintf "%s\t" indent) src)
    | Scale (p, scad) ->
      Printf.sprintf
        "%sscale(%s)\n%s"
        indent
        (Vec3.to_string p)
        (print (Printf.sprintf "%s\t" indent) scad)
    | Resize (p, scad) ->
      Printf.sprintf
        "%sresize(%s)\n%s"
        indent
        (Vec3.to_string p)
        (print (Printf.sprintf "%s\t" indent) scad)
    | Offset { src; offset } ->
      Printf.sprintf
        "%soffset(%s)\n%s"
        indent
        ( match offset with
        | `Radius r  -> Printf.sprintf "r = %f" r
        | `Delta d   -> Printf.sprintf "delta = %f" d
        | `Chamfer d -> Printf.sprintf "delta = %f, chamfer=true" d )
        (print (Printf.sprintf "%s\t" indent) src)
    | Import { file; convexity; dxf_layer } ->
      Printf.sprintf
        "%simport(\"%s\", convexity=%i%s);\n"
        indent
        file
        convexity
        (maybe_fmt ", layer=%s" dxf_layer)
    | Surface { file; center; invert; convexity } ->
      Printf.sprintf
        "%ssurface(\"%s\", center=%B, invert=%B, convexity=%i);\n"
        indent
        file
        center
        invert
        convexity
    | Color { src; color; alpha } ->
      Printf.sprintf
        "%scolor(%s%s)\n%s"
        indent
        (Color.to_string color)
        (maybe_fmt ", alpha=%f" alpha)
        (print (Printf.sprintf "%s\t" indent) src)
    | Render { src; convexity } ->
      Printf.sprintf
        "%srender(convexity=%i)\n%s"
        indent
        convexity
        (print (Printf.sprintf "%s\t" indent) src)
  in
  print "" (unpack t)

let to_file path t =
  let oc = open_out path in
  Printf.fprintf oc "%s" (to_string t);
  close_out oc

exception FailedExport = Export.FailedExport

let export (type a b) path (t : (a, b) t) =
  let space, allowed =
    match t with
    | D2 _ -> "2D", Export.d2_exts
    | D3 _ -> "3D", Export.d3_exts
  in
  match Export.legal_ext allowed path with
  | Ok ()     ->
    let temp = Filename.temp_file "out" ".scad" in
    to_file temp t;
    Export.script path temp
  | Error ext ->
    invalid_arg (Printf.sprintf "%s files are not supported for %s export." ext space)

module Infix = struct
  let ( |>> ) t p = translate p t
  let ( |@> ) t r = rotate r t
end
