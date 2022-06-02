open Scad_ml

let () =
  let scad =
    Poly2.square ~center:true (v2 3. 3.)
    |> Mesh.linear_extrude
         ~slices:100
         ~scale:(v2 4. 4.)
         ~twist:(2. *. Float.pi)
         ~center:false
         ~height:10.
    |> Mesh.to_scad
  and oc = open_out "poly_linear_extrude.scad" in
  Scad.write oc scad;
  close_out oc
