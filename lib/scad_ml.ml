(** {1 OpenSCAD DSL} *)

module Scad = Scad

(** {2 Configuration Types}

    These modules are provided at the top-level as convenient namespaces for
    the configuration types taken as parameters to their corresponding model
    building functions in {!module:Scad}. *)

(** {!Scad.text} configuration parameter types *)
module Text : sig
  (** Horizontal alignment *)
  type h_align = Text.h_align =
    | Left
    | Center
    | Right

  (** Vertical alignment *)
  type v_align = Text.v_align =
    | Top
    | Center
    | Baseline

  (** Reading direction *)
  type direction = Text.direction =
    | LeftToRight
    | RightToLeft
    | TopToBottom
    | BottomToTop
end =
  Text

(** {!Scad.color} specification type

   A selection of hardcoded colours are available, along with aribtrary colours
   by way of with the [RGB of float * float * float] and [Hex of string]
   constructors.

   - As in OpenSCAD, [RGB (r, g, b)] values are given as floats in the range of
     [0.] to [1.], rather than the more traditional integers.
   - [Hex v] values can be given in four formats: ["#rgb"], ["#rgba"],
     ["#rgba"], and ["#rrggbbaa"]. If alpha is given both in the hex value, and
     in the [?alpha] parameter to {!Scad.color}, the parameter will take
     precedence. *)
module Color = Color

(** {1 Vectors}

    Spatial vectors used to transform {!Scad.t} shapes, and compose into
    other types ({i e.g.} {!Path3.t}, {!Poly2.t}, and {!Mesh.t}) contained in
    the modules below, which can in turn used to generate point based shapes to
    be mapped into {!Scad.t}. *)

(** 3-dimensional vector *)
type v3 = Vec.v3 =
  { x : float
  ; y : float
  ; z : float
  }

(** 2-dimensional vector *)
type v2 = Vec.v2 =
  { x : float
  ; y : float
  }

(** [v2 x y]

    Construct a 2d vector from [x] and [y] coordinates. *)
let v2 : float -> float -> v2 = Vec.v2

(** [v3 x y z]

    Construct a 3d vector from [x], [y], and [z] coordinates. *)
let v3 : float -> float -> float -> v3 = Vec.v3

(** 2-dimensional vector type, including basic mathematical/geometrical
    operations and transformations mirroring those found in {!module:Scad},
    allowing for points in 2d space, and higher level types composed of them
    ({i e.g.} {!Path2.t} and {!Poly2.t}) to be manipulated in
    similar fashion to 2d OpenSCAD shapes ({!Scad.d2}). *)
module Vec2 = struct
  include Vec2 (** @inline *)

  (** [affine m t]

      Apply 2d affine transformation matrix [m] to the vector [t]. *)
  let affine m t = Affine2.transform m t

  (** {1 2d to 3d transformations} *)

  (** [affine3 m t]

      Apply 3d affine transformation matrix [m] to the vector [t], taking it into
      the 3rd dimension. *)
  let affine3 m { x; y } = Affine3.transform m (v3 x y 0.)

  (** [quaternion ?about q t]

      Rotate [t] with the quaternion [q] around the origin (or the point [about]
      if provided), taking it into the 3rd dimension. *)
  let quaternion ?about q { x; y } = Quaternion.transform ?about q (v3 x y 0.)

  (** [axis_rotate ?about ax a t]

      Rotates the vector [t] around the axis [ax] through the origin (or the
      point [about] if provided) by the angle [a], taking it into the third
      dimension. *)
  let axis_rotate ?about ax a { x; y } =
    Quaternion.(transform ?about (make ax a) (v3 x y 0.))
end

(** 3-dimensional vector type, including basic mathematical/geometrical
    operations and transformations mirroring those found in {!module:Scad},
    allowing for points in 3d space, and higher level types composed of them
    ({i e.g.} {!Path3.t}, {!Poly3.t}, and {!Mesh.t}) to be manipulated in
    similar fashion to 3d OpenSCAD shapes ({!Scad.d3}). *)
module Vec3 = struct
  include Vec3 (** @inline *)

  (** {1 Additional 3d transformations} *)

  (** [affine m t]

      Apply affine transformation matrix [m] to the vector [t]. *)
  let[@inline] affine m t = Affine3.transform m t

  (** [quaternion ?about q t]

      Rotate [t] with the quaternion [q] around the origin (or the point [about]
      if provided). *)
  let[@inline] quaternion ?about q t = Quaternion.transform ?about q t

  (** [axis_rotate ax a t]

      Rotates the vector [t] around the axis [ax] through the origin (or the
      point [about] if provided) by the angle [a]. *)
  let axis_rotate ?about ax a = Quaternion.(transform ?about (make ax a))
end

(** {1 Transformations} *)

module Affine2 = Affine2

(** Affine transformation matrices for transforming 3d vectors ({!Vec3.t}), and
   3d shapes ({!Scad.t}) via OpenSCAD's own
   {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language#multmatrix}multmatrix},
   (see {!Scad.affine}). *)
module Affine3 = struct
  include Affine3 (** @inline *)

  (** {1 Conversions} *)

  (** [project t]

      Project [t] down into a 2d affine transformation matrix (z axis components
      dropped). *)
  let project (t : t) =
    Affine2.
      { r0c0 = t.r0c0
      ; r0c1 = t.r0c1
      ; r0c2 = t.r0c3
      ; r1c0 = t.r1c0
      ; r1c1 = t.r1c1
      ; r1c2 = t.r1c3
      ; r2c0 = 0.
      ; r2c1 = 0.
      ; r2c2 = t.r3c3
      }

  (** [of_quaternion q]

    Create an affine transformation matrix equivalent to the quaternion [q]. *)
  let of_quaternion ?trans q = Quaternion.to_affine ?trans q
end

module Quaternion = Quaternion
module Plane = Plane

(** {1 2-dimensional paths and polygons} *)

module Path2 = Path2
module Bezier2 = Bezier2
module CubicSpline = CubicSpline
module Poly2 = Poly2
module PolyText = PolyText

(** {1 3-dimensional paths, coplanar polygons, and meshes} *)

module Path3 = Path3
module Bezier3 = Bezier3
module Poly3 = Poly3
module Mesh = Mesh

(** {1 Utilities} *)

module Math = Math
module Easing = Easing
module Export = Export
module BallTree2 = BallTree2
module BallTree3 = BallTree3
