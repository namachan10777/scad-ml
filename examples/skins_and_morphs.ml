(** {0 Skins and Morphs} *)

open Scad_ml

let () =
  let sq = Path3.square ~center:true (v2 2. 2.)
  and circ =
    Path3.circle ~fn:36 0.6
    |> Path3.rotate (v3 0. (Float.pi /. 4.) 0.)
    |> Path3.translate (v3 0. 0. 1.)
  and ellipse =
    Path3.ellipse ~fn:21 (v2 0.5 1.5)
    |> Path3.rotate (v3 0. (Float.pi /. 1.5) 0.)
    |> Path3.translate (v3 3. 0. 1.)
  and rect =
    Path3.square ~center:true (v2 0.2 1.) |> Path3.translate (v3 4. 0. 0.) |> List.rev
  and circ2 =
    Path3.circle ~fn:50 1.
    |> Path3.rotate (v3 0. (Float.pi /. 2.) 0.)
    |> Path3.translate (v3 2. 1. (-2.))
    |> List.rev
  and ellipse2 =
    Path3.ellipse ~fn:60 (v2 0.25 0.5)
    |> Path3.rotate (v3 0. (Float.pi /. 1.5) 0.)
    |> Path3.translate (v3 1. 0. (-2.))
    |> List.rev
  and sq2 =
    Path3.square ~center:true (v2 1. 1.)
    |> Path3.rotate (v3 0. 0. (Float.pi /. 4.))
    |> Path3.translate (v3 0. 0. (-1.))
  in
  ignore [ sq; circ; ellipse; rect; circ2; ellipse2; sq2 ];
  Skin.skin
    ~refine:2
    ~endcaps:`Loop
    ~slices:(`Flat 35)
    ~spec:
      (`Mix
        [ `Reindex `ByLen
        ; `Reindex `ByLen
        ; `Reindex `ByLen
        ; `Reindex `ByLen
        ; `Reindex `ByLen
        ; `Reindex `BySeg
        ; `Distance
        ; `Direct `BySeg
        ] )
    [ sq
    ; circ
    ; ellipse
    ; rect
    ; circ2
    ; ellipse2
    ; sq2
    ; Path3.translate (v3 0. 0. (-0.5)) sq
    ]
  |> Mesh.merge_points
  |> Mesh.to_scad
  |> fun s -> Scad.union [ s; Scad.sphere 1. ] |> Scad.to_file "skin_test.scad"

(* let () = *)
(*   let rounded = *)
(*     Path3.( *)
(*       roundover ~fn:32 *)
(*       @@ Round.flat *)
(*            ~closed:true *)
(*            ~corner:(Round.circ (`Radius 15.)) *)
(*            (v3 (-25.) 25. 0. :: Path3.square (v2 50. 50.))) *)
(*     |> Path3.subdivide ~closed:true ~freq:(`Refine (4, `ByLen)) *)
(*   and sq = *)
(*     v3 4. 0. 0. :: Path3.square ~center:true (v2 10. 10.) *)
(*     |> Path3.translate (v3 30. 30. 10.) *)
(*   in *)
(*   Skin.skin ~refine:2 ~slices:(`Flat 25) ~spec:(`Flat `Tangent) [ rounded; sq ] *)
(*   |> Mesh.to_scad *)
(*   |> Scad.to_file "tangent_skin_test.scad" *)

(* NOTE: need to test this simple case with BOSL2 and see where my tangent_match
   diverges. From looking at the two of them, they seem the same, but the point
    duplication is leading to 2 or 3 times (exactly) the number of desired
    points. *)

let () =
  Skin.skin
    ~refine:2
    ~slices:(`Flat 25)
    ~spec:(`Flat `Tangent)
    Path3.[ translate (v3 0. 0. 3.) @@ circle ~fn:80 2.; circle ~fn:5 4. ]
  |> Mesh.to_scad
  |> Scad.to_file "tangent_skin_test.scad"

(* let () = *)
(*   Scad.union *)
(*     [ Scad.circle ~fn:5 1. |> Scad.linear_extrude ~height:1. *)
(*     ; Poly2.circle ~fn:5 1. *)
(*       |> Mesh.linear_extrude ~height:1. *)
(*       |> Mesh.translate (v3 0. 0. 1.5) *)
(*       |> Mesh.to_scad *)
(*     ] *)
(*   |> Scad.to_file "circ_test.scad" *)
