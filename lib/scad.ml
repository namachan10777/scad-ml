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
      ; height : float option
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
  | Render of
      { src : scad
      ; convexity : int
      }

type 'space t =
  | D2 : scad -> two_d t
  | D3 : scad -> three_d t

type d2 = two_d t
type d3 = three_d t

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

let empty_exn n =
  invalid_arg
    (Printf.sprintf
       "List must be non-empty. Use %s_2d or %s_3d if empty lists are expected."
       n
       n )

let union : type a. a t list -> a t =
 fun ts ->
  match ts with
  | D2 _ :: _ -> union_2d ts
  | D3 _ :: _ -> union_3d ts
  | []        -> empty_exn "union"

let minkowski_2d ts = d2 @@ Minkowski (List.map unpack ts)
let minkowski_3d ts = d3 @@ Minkowski (List.map unpack ts)

let minkowski : type a. a t list -> a t =
 fun ts ->
  match ts with
  | D2 _ :: _ -> minkowski_2d ts
  | D3 _ :: _ -> minkowski_3d ts
  | []        -> empty_exn "minkowski"

let hull_2d ts = d2 @@ Hull (List.map unpack ts)
let hull_3d ts = d3 @@ Hull (List.map unpack ts)

let hull : type a. a t list -> a t =
 fun ts ->
  match ts with
  | D2 _ :: _ -> hull_2d ts
  | D3 _ :: _ -> hull_3d ts
  | []        -> empty_exn "hull"

let difference (type a) (t : a t) (sub : a t list) =
  map (fun scad -> Difference (scad, List.map unpack sub)) t

let intersection_2d ts = d2 @@ Intersection (List.map unpack ts)
let intersection_3d ts = d3 @@ Intersection (List.map unpack ts)

let intersection : type a. a t list -> a t =
 fun ts ->
  match ts with
  | D2 _ :: _ -> intersection_2d ts
  | D3 _ :: _ -> intersection_3d ts
  | []        -> empty_exn "intersection"

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
    ?(scale = Vec2.v 1. 1.)
    ?(fn = 16)
    (D2 src)
  =
  d3 @@ LinearExtrude { src; height; center; convexity; twist; slices; scale; fn }

let rotate_extrude ?angle ?(convexity = 10) ?fa ?fs ?fn (D2 src) =
  d3 @@ RotateExtrude { src; angle; convexity; fa; fs; fn }

let scale factors = map (fun scad -> Scale (factors, scad))
let resize new_dims = map (fun scad -> Resize (new_dims, scad))
let offset offset (D2 src) = d2 @@ Offset { src; offset }
let import ?dxf_layer ?(convexity = 10) file = Import { file; convexity; dxf_layer }

let legal_ext allowed file =
  let ext =
    let len = String.length file in
    String.uncapitalize_ascii @@ String.sub file (len - 3) 3
  in
  let rec aux = function
    | h :: t -> if String.equal ext h then Ok () else aux t
    | []     -> Error ext
  in
  aux allowed

let import_2d ?dxf_layer ?convexity file =
  match legal_ext [ "dxf"; "svg" ] file with
  | Ok ()     -> d2 (import ?dxf_layer ?convexity file)
  | Error ext ->
    invalid_arg
      (Printf.sprintf "Input file extension %s is not supported for 2D import." ext)

let import_3d ?convexity file =
  match legal_ext [ "stl"; "off"; "amf"; "3mf" ] file with
  | Ok ()     -> d3 (import ?convexity file)
  | Error ext ->
    invalid_arg
      (Printf.sprintf "Input file extension %s is not supported for 3D import." ext)

let color ?alpha color = map (fun src -> Color { src; color; alpha })
let render ?(convexity = 10) = map (fun src -> Render { src; convexity })

let to_string t =
  let value_map = Util.value_map_opt
  and deg_of_rad r = 180.0 *. r /. Float.pi
  and buf_add_list b f = function
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
  and maybe_fmt fmt opt = value_map (Printf.sprintf fmt) ~default:"" opt
  and string_of_f_ fa fs (fn : int option) =
    Printf.sprintf
      "%s%s%s"
      (value_map ~default:"" (fun fa -> Printf.sprintf ", $fa=%f" @@ deg_of_rad fa) fa)
      (value_map ~default:"" (Printf.sprintf ", $fs=%f") fs)
      (value_map ~default:"" (Printf.sprintf ", $fn=%i") fn)
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
        (deg_of_rad r)
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
        "%slinear_extrude(%scenter=%B, convexity=%d, %sslices=%d, scale=[%f, %f], $fn=%d)\n\
         %s"
        indent
        (maybe_fmt "height=%f, " height)
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
        (maybe_fmt "angle=%f" @@ Option.map deg_of_rad angle)
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

let write oc t =
  Printf.fprintf oc "%s" (to_string t);
  flush oc

module Infix = struct
  let ( |>> ) t p = translate p t
  let ( |@> ) t r = rotate r t
end
