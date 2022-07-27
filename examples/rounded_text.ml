(** {0 Rounded text extrusion} *)
open Scad_ml

(** Generate a list of {{!Scad_ml.Poly2.t} [Poly2.t]} spelling out {b Hello
    World!}. At the moment, {{!Scad_ml.PolyText.text} [PolyText.text]} is not
    as flexible and feature rich as {{!Scad_ml.Scad.text} [Scad.text]}
    (OpenSCADs text shape function), but this gives up point representations
    that be can work with directly. *)
let hello = PolyText.text ~center:true ~fn:5 ~size:5. ~font:"Ubuntu" "Hello!"

(** Circular roundovers with [fn] steps, specified by a distance to be [`Cut]
    off of the corners. You can expect some finickiness with applying
    roundovers to the polygons produced by {{!Scad_ml.PolyText.text}
    [PolyText.text]}, as the paths coming from Cairo may have some points quite
    close together, leading to illegal paths when further roundover operations
    are applied. *)
let caps =
  let spec = Mesh.Cap.(round ~mode:Delta @@ circ ~fn:5 (`Cut 0.025)) in
  Mesh.Cap.{ top = spec; bot = spec }

(** Map over the character polys in [hello] with a rounded extrusion funcion
    specified by [caps], and convert into {{!Scad_ml.Scad.t} [Scad.t]}s that we
    can union to create our final model. *)

let extruder poly = Mesh.(to_scad @@ linear_extrude ~caps ~height:0.5 poly)
let () = List.map extruder hello |> Scad.union |> Scad.to_file "rounded_text.scad"

(** {%html:
    <p style="text-align:center;">
    <img src="../assets/rounded_text.png" style="width:150mm;"/>
    </p> %}
    *)
