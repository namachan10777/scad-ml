# OpenSCAD DSL for OCaml

## Overview
This library provides an OCaml front-end to the
[OpenSCAD](https://openscad.org/) solid modelling language. All SCAD primitives
and transformation functions are made available, with the major notable
difference being that angles are represented in radians (and converted to
degrees when compiling to OpenSCAD).

## Usage
``` ocaml
open Scad_ml

let scad_logo =
  let rad = 5.
  and fn = 720 in
  let cyl = Scad.cylinder ~fn ~center:true (rad /. 2.) (rad *. 2.3) in
  let cross_cyl = Scad.rotate (0., Float.pi /. 2., 0.) cyl in
  Scad.union
    [ Scad.difference
        (Scad.sphere ~fn rad)
        [ cyl; cross_cyl; Scad.rotate (0., 0., Float.pi /. 2.) cross_cyl ]
    ; Scad.color ~alpha:0.25 Color.Magenta cross_cyl
    ]

let () =
  let oc = open_out "/path/to/things/scad_logo.scad"
  Scad.write oc scad_logo;
  close_out oc
```
![OpenSCAD logo](images/scad_logo.png)
Generated scads can then be viewed with the [OpenSCAD
viewer](https://openscad.org/downloads.html) as you normally would.

## Documentation
Online documentation is available
[here](https://geoffder.github.io/scad-ml/scad_ml/Scad_ml/index.html).

## Companion PPX
There is a companion ppx, [\[@@deriving
scad\]](https://github.com/geoffder/ppx_deriving_scad) for generating
transformation functions for user defined records made up of the `Scad.t` and
`Vec3.t` types provided in this library.

# License
BSL-1.0
