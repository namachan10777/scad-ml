type t = Core.scad_t

let cylinder ?(center=false) ?fa ?fs ?fn r h =
    Core.Cylinder { r1=r; r2=r; h=h; center=center; fa; fs; fn }

let cube ?(center=false) size =
    Core.Cube { size=size; center }

let sphere ?fa ?fs ?fn r = Core.Sphere { r; fa; fs; fn }

let translate p scad =
    Core.Translate (p, scad)

let rotate r scad =
    Core.Rotate (r, scad)

let union elements =
    Core.Union elements

let minkowski elements =
    Core.Minkowski elements

let hull elements =
    Core.Hull elements

let difference min sub =
    Core.Difference (min, sub)

let intersection elements =
    Core.Intersection elements

let polyhedron points faces =
    Core.Polyhedron (points, faces)

let mirror v scad =
    Core.Mirror (v, scad)

let projection ?(cut=false) src =
    Core.Projection { src; cut }

let linear_extrude ?height ?(center=false) ?(convexity=10) ?twist ?(slices=20) ?(scale=1.0) ?(fn=16) src =
    Core.LinearExtrude { src; height; center; convexity; twist; slices; scale; fn }
