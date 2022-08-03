## v2.0.0

- Extension of the `Scad.t` GADT to prevent 3d transformation of 2d shapes
- `Vec3.t` (and now `Vec2.t`) types are records rather than tuples and have
  the respective modules have been extended with additional geometrical
  operations
- Add `cone`, `to_file`, `export`, and `snapshot` to `Scad` module
- `Scad.offset` now takes a `?mode` parameter, rather than variant with value
  payload (from ``Scad.offset (`Radius r)`` to ``Scad.offset ~mode:`Radius r``)
- New modules (`2` and `3` suffixes indicate dimensionality):
  * `Plane` -- normalized cartesian plane operations
  * `Path2`/`Path3` -- generation and manipulation of paths of points
  * `Poly2`/`Poly3` -- planar polygons (outer, and zero or more inner paths)
  * `PolyText` -- extracting point representations of text from fonts (via Cairo)
  * `Bezier2`/`Bezier3` -- generation and manipulation of bezier curves
  * `CublicSpline` -- cubic spline interpolation of 2d paths
  * `Mesh` -- points and faces 3d mesh generation and manipulation (polyhedrons)
  * `Math` -- float and 2d matrix operations
  * `Export` -- exporting `.scad` scripts via OpenSCAD CLI
  * `BallTree2`/`BallTree3` -- vector space partitioning search trees

## v1.1.0

- Added support for OpenSCAD `render`

## v1.0.0

- Initial opam release of the Scad_ml library
