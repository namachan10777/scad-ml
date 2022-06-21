# OpenSCAD DSL for OCaml

## Overview

This library provides an OCaml front-end to the
[OpenSCAD](https://openscad.org/) solid modelling language. All SCAD primitives
and transformation functions are made available.

## Notable differences from the OpenSCAD language

- Angles are represented in radians (and converted to degrees when compiling to
  OpenSCAD).
- The dimensional system (2D or 3D) each shape inhabits is tracked by the type
  system. This is used to restrict the operations that can be legally applied
  (e.g. 2D shapes cannot be moved off of the xy plane, `linear_extrude` can only
  be applied to 2D shapes) and enforcing non-mixing of 2D and 3D shapes during
  boolean operations.
- Additional modules providing sweeps, roundovers, and more, inspired largely by
  [BOSL2](https://github.com/revarbat/BOSL2), but also other OpenSCAD libraries such
  as [dotSCAD](https://github.com/JustinSDK/dotSCAD/tree/master/src), and
  [PlotFunction](https://github.com/rcolyer/plot-function).

## Usage

``` ocaml
open Scad_ml

let () =
  let scad_logo =
    let rad = 5.
    and fn = 720 in
    let cyl = Scad.cylinder ~fn ~center:true ~height:(rad *. 2.3) (rad /. 2.) in
    let cross_cyl = Scad.rotate (v3 0. (Float.pi /. 2.) 0.) cyl in
    Scad.union
      [ Scad.difference
          (Scad.sphere ~fn rad)
          [ cyl; cross_cyl; Scad.rotate (v3 0. 0. (Float.pi /. 2.)) cross_cyl ]
      ; Scad.color ~alpha:0.25 Color.Magenta cross_cyl
      ]
  in
  Scad.to_file "scad_logo.scad" scad_logo
```

![OpenSCAD logo](docs/assets/scad_logo.png)
Generated scads can then be viewed with the [OpenSCAD
viewer](https://openscad.org/downloads.html) as you normally would.

## Documentation

Online documentation is available
[here](https://namachan10777.github.io/scad-ml/scad_ml/Scad_ml/index.html).

## Companion PPX

There is a companion ppx, [\[@@deriving
scad\]](https://github.com/geoffder/ppx_deriving_scad) for generating
transformation functions for user-defined records and abstract types made up of
the `Scad.t`, along with their corresponding vector (`Vec2.t` or `Vec3.t`) types
(and those composed of them) provided in this library.

## Authors

- [@namachan10777](https://github.com/namachan10777)
  - Original author of core DSL
- [@geoffder](https://github.com/geoffder)
  - Overhauled DSL with dimensional types
  - Addional vector transformation modules (*e.g.* MultMatrix, Quaternion)
  - Modules implementing 2d/3d vector point representations of shapes and
    utilities operating upon them to enable sweeps, roundovers, etc.

## License

BSL-1.0
