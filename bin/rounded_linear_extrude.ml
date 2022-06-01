open Scad_ml

let () =
  let scad =
    let shape = Path2.square ~center:true (v2 3. 3.) in
    Path2.(roundover ~fn:30 Round.(flat ~corner:(chamf (`Cut 0.5)) shape))
    |> List.map (Vec2.translate (v2 1.5 1.5))
    |> Poly2.make
    |> Mesh.(
         linear_extrude
           ~slices:100
           ~scale:(v2 4. 4.)
           ~twist:(2. *. Float.pi)
           ~center:false
           ~caps:
             Cap.{ top = round @@ bez (`Joint 1.); bot = round @@ circ (`Radius (-0.5)) }
           ~height:10.)
    |> Mesh.to_scad
  and oc = open_out "rounded_linear_extrude.scad" in
  Scad.write oc scad;
  close_out oc
