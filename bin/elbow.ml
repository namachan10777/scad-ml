open Scad_ml

let poly =
  [ -10., -1.; -10., 6.; -7., 6.; -7., 1.; 7., 1.; 7., 6.; 10., 6.; 10., -1. ]
  |> Path2.of_tups
  |> Poly2.make
