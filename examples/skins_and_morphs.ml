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
  in
  ignore [ sq; circ; ellipse; rect; circ2; ellipse2 ];
  Skin.skin
    ~refine:2
    ~endcaps:`Loop
    ~slices:(`Flat 50)
    ~spec:(`Flat (`Reindex `ByLen))
    [ sq; circ; ellipse; rect; circ2; ellipse2 ]
  |> Mesh.to_scad
  |> fun s -> Scad.union [ s; Scad.sphere 1. ] |> Scad.to_file "skin_test.scad"
