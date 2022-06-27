(** {0 Path transfomations (standard vs euler)} *)
open Scad_ml

let () =
  let poly =
    [ -10., -1.; -10., 6.; -7., 6.; -7., 1.; 7., 1.; 7., 6.; 10., 6.; 10., -1. ]
    |> Path2.of_tups
    |> Poly2.make
  in
  let paths =
    let d = 20.
    and p { x; y; z } = [ v3 x y z; v3 (x *. 2.) (y *. 2.) (z *. 2.) ] in
    let out = [ v3 d 0. 0.; v3 d 0. d; v3 d 0. (-.d) ] in
    let f i =
      List.map (fun s -> Vec3.rotate (v3 0. 0. Float.(pi /. 4. *. i)) s |> p) out
    in
    p (v3 0. 0. d) :: p (v3 0. 0. (-.d)) :: List.concat_map f (List.init 8 Float.of_int)
  and flat = Scad.linear_extrude ~height:1. (Scad.polygon poly.outer) in
  let build euler =
    let scad =
      let f path = Mesh.(to_scad @@ path_extrude ~euler ~path poly) in
      Scad.union @@ (flat :: List.map f paths)
    and name =
      Printf.sprintf "sweep_starburst_%s.scad" (if euler then "euler" else "default")
    in
    Scad.to_file name scad
  in
  build false;
  build true
