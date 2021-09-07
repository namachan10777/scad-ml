type t = Scad.t

let cylinder ?(center = false) ?fa ?fs ?fn r h =
  Scad.Cylinder { r1 = r; r2 = r; h; center; fa; fs; fn }

let cube ?(center = false) size = Scad.Cube { size; center }
let sphere ?fa ?fs ?fn r = Scad.Sphere { r; fa; fs; fn }
let square ?(center = false) size = Scad.Square { size; center }
let circle ?fa ?fs ?fn r = Scad.Circle { r; fa; fs; fn }
let polygon ?(convexity = 10) ?paths points = Scad.Polygon { points; paths; convexity }

let text ?size ?font ?halign ?valign ?spacing ?direction ?language ?script ?fn str =
  Scad.Text
    { text = str; size; font; halign; valign; spacing; direction; language; script; fn }

let translate p scad = Scad.Translate (p, scad)
let rotate r scad = Scad.Rotate (r, scad)
let rotate_about_pt r p scad = translate p scad |> rotate r |> translate (Vec3.negate p)
let vector_rotate ax r scad = Scad.VectorRotate (ax, r, scad)

let vector_rotate_about_pt ax r p scad =
  translate p scad |> vector_rotate ax r |> translate (Vec3.negate p)

let multmatrix mat scad = Scad.MultMatrix (mat, scad)
let quaternion q scad = Scad.MultMatrix (Quaternion.to_multmatrix q, scad)

let quaternion_about_pt q p scad =
  translate p scad |> quaternion q |> translate (Vec3.negate p)

let union elements = Scad.Union elements
let minkowski elements = Scad.Minkowski elements
let hull elements = Scad.Hull elements
let difference min sub = Scad.Difference (min, sub)
let intersection elements = Scad.Intersection elements
let polyhedron points faces = Scad.Polyhedron (points, faces)
let mirror v scad = Scad.Mirror (v, scad)
let projection ?(cut = false) src = Scad.Projection { src; cut }

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
  Scad.LinearExtrude { src; height; center; convexity; twist; slices; scale; fn }

let rotate_extrude ?angle ?(convexity = 10) ?fa ?fs ?fn src =
  Scad.RotateExtrude { src; angle; convexity; fa; fs; fn }

let scale ratios scad = Scad.Scale (ratios, scad)
let resize new_dims scad = Scad.Resize (new_dims, scad)
let offset ?(chamfer = false) offset src = Scad.Offset { src; offset; chamfer }
let import ?dxf_layer ?(convexity = 10) file = Scad.Import { file; convexity; dxf_layer }
let color ?alpha color src = Scad.Color { src; color; alpha }
let ( |>> ) scad p = translate p scad
let ( |@> ) scad r = rotate r scad
