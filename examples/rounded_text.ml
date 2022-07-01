(** {0 Rounded text extrusion} *)
open Scad_ml

let hello = PolyText.text ~center:true ~fn:3 ~size:5. ~font:"Roboto" "Hello World!"

let caps =
  Mesh.Cap.
    { top = round ~mode:Delta @@ circ ~fn:5 (`Cut 0.03)
    ; bot = round ~mode:Delta @@ circ ~fn:5 (`Cut 0.03)
    }

let extruder poly = Mesh.(to_scad @@ linear_extrude ~caps ~height:0.5 poly)
let () = List.map extruder hello |> Scad.union |> Scad.to_file "rounded_text.scad"

(** {%html:
    <p style="text-align:center;">
    <img src="../assets/rounded_text.png" style="width:150mm;"/>
    </p> %}
    *)
