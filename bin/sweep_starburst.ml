open Scad_ml

let () =
  let paths =
    let d = 20.
    and p { x; y; z } = [ v3 x y z; v3 (x *. 2.) (y *. 2.) (z *. 2.) ] in
    let out = [ v3 d 0. 0.; v3 d 0. d; v3 d 0. (-.d) ] in
    let f i =
      List.map (fun s -> Vec3.rotate (v3 0. 0. Float.(pi /. 4. *. i)) s |> p) out
    in
    p (v3 0. 0. d) :: p (v3 0. 0. (-.d)) :: List.concat_map f (List.init 8 Float.of_int)
  and flat = Scad.polygon Elbow.poly.outer |> Scad.linear_extrude ~height:1. in
  let build euler =
    let scad =
      let f path =
        let transforms = Path3.to_transforms ~euler path in
        Mesh.to_scad @@ Mesh.sweep ~transforms Elbow.poly
      in
      Scad.union @@ (flat :: List.map f paths)
    and oc =
      open_out
        (Printf.sprintf "sweep_starburst_%s.scad" (if euler then "euler" else "default"))
    in
    Scad.write oc scad;
    close_out oc
  in
  build false;
  build true
