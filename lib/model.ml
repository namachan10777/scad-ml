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
let rotate_about_pt r p scad = translate p scad |> rotate r |> translate (Math.negate p)
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
