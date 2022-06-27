(** {0 Rounded text extrusion} *)
open Scad_ml

let () =
  let scad =
    let hello = PolyText.text ~center:true ~fn:5 ~size:5. ~font:"Roboto" "Hello World!"
    and f poly =
      let mesh =
        Mesh.(
          linear_extrude
            ~check_valid:(`Quality 1)
            ~caps:
              Cap.
                { top = round ~mode:Delta @@ circ ~fn:5 (`Cut 0.01)
                ; bot = round ~mode:Delta @@ circ ~fn:5 (`Cut 0.01)
                }
              (* ~caps: *)
              (*   Spec. *)
              (*     { top = round @@ chamf ~height:0.1 () *)
              (*     ; bot = round @@ chamf ~height:0.1 () *)
              (*     } *)
            ~height:0.5
            poly)
      in
      (* Scad.union [ Mesh.to_scad mesh; Mesh.show_points (fun _ -> Scad.sphere 0.05) mesh ] *)
      Mesh.to_scad mesh
    in
    Scad.union @@ List.map f hello
  in
  Scad.to_file "rounded_text.scad" scad
