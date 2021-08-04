type t = Core.scad_t

let cylinder ?(center = false) ?fa ?fs ?fn r h =
  Core.Cylinder { r1 = r; r2 = r; h; center; fa; fs; fn }

let cube ?(center = false) size = Core.Cube { size; center }
let sphere ?fa ?fs ?fn r = Core.Sphere { r; fa; fs; fn }
let square ?(center = false) size = Core.Square { size; center }
let circle ?fa ?fs ?fn r = Core.Circle { r; fa; fs; fn }
let polygon ?(convexity = 10) ?paths points = Core.Polygon { points; paths; convexity }

let text ?size ?font ?halign ?valign ?spacing ?direction ?language ?script ?fn str =
  Core.Text
    { text = str; size; font; halign; valign; spacing; direction; language; script; fn }

let translate p scad = Core.Translate (p, scad)
let rotate r scad = Core.Rotate (r, scad)
let rotate_about_pt r p scad = translate p scad |> rotate r |> translate (Vec3.negate p)
let vector_rotate ax r scad = Core.VectorRotate (ax, r, scad)

let vector_rotate_about_pt ax r p scad =
  translate p scad |> vector_rotate ax r |> translate (Vec3.negate p)

let multmatrix mat scad = Core.MultMatrix (mat, scad)
let quaternion q scad = Core.MultMatrix (Quaternion.to_multmatrix q, scad)

let quaternion_about_pt q p scad =
  translate p scad |> quaternion q |> translate (Vec3.negate p)

let union elements = Core.Union elements
let minkowski elements = Core.Minkowski elements
let hull elements = Core.Hull elements
let difference min sub = Core.Difference (min, sub)
let intersection elements = Core.Intersection elements
let polyhedron points faces = Core.Polyhedron (points, faces)
let mirror v scad = Core.Mirror (v, scad)
let projection ?(cut = false) src = Core.Projection { src; cut }

let linear_extrude
    ?height
    ?(center = false)
    ?(convexity = 10)
    ?twist
    ?(slices = 20)
    ?(scale = 1.0)
    ?(fn = 16)
    src
  =
  Core.LinearExtrude { src; height; center; convexity; twist; slices; scale; fn }

let rotate_extrude ?angle ?(convexity = 10) ?fa ?fs ?fn src =
  Core.RotateExtrude { src; angle; convexity; fa; fs; fn }

let scale ratios scad = Core.Scale (ratios, scad)
let resize new_dims scad = Core.Resize (new_dims, scad)
let offset ?(chamfer = false) offset src = Core.Offset { src; offset; chamfer }
let import ?dxf_layer ?(convexity = 10) file = Core.Import { file; convexity; dxf_layer }
let ( |>> ) scad p = translate p scad
let ( |@> ) scad r = rotate r scad
