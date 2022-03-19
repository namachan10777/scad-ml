module Scad = Scad
module Text = Text
module Color = Color
module Vec2 = Vec2
module Quaternion = Quaternion
module Bezier2 = Bezier2
module Bezier3 = Bezier3
module CubicSpline = CubicSpline
module Poly2 = Poly2
module Mesh = Mesh
module PolyRound = PolyRound
module Rounding2 = Rounding2
module Path2 = Path2
module Path3 = Path3
module RoundExtrude = RoundExtrude
module Math = Math
module Plane = Plane
module PolyText = PolyText

type v3 = Vec.v3 =
  { x : float
  ; y : float
  ; z : float
  }

type v2 = Vec.v2 =
  { x : float
  ; y : float
  }

let v2 = Vec.v2
let v3 = Vec.v3

(** A rotation matrix.

    Used in conjunction with {!module:Quaternion} to provide an additional means
    of rotating OpenSCAD objects and vectors ({!Vec3.t}). *)
module RotMatrix = struct
  include RotMatrix

  (** [align a b]

    Calculate a rotation matrix that would bring [a] into alignment with [b]. *)
  let align a b = Quaternion.(to_rotmatrix @@ alignment a b)
end

(** An affine transformation matrix.

    To be used with OpenSCADs
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language#multmatrix}multmatrix},
    which is applied in this library with {!Scad.multmatrix}. *)
module MultMatrix = struct
  include MultMatrix

  (** [rotation r]

    Create an affine transformation matrix from the euler angle vector [r]. *)
  let rotation r = Quaternion.(to_multmatrix @@ of_euler r)

  (** [vector_rotation ax r]

    Create an affine transformation matrix representing a rotation of the angle
    [r] around the axis [ax]. *)
  let vector_rotation ax r = Quaternion.(to_multmatrix @@ make ax r)

  (** [of_quaternion q]

    Create an affine transformation matrix equivalent to the quaternion [q]. *)
  let of_quaternion q = Quaternion.to_multmatrix q
end

(** 3-dimensional vector type.

    In addition to basic math and vector operations, relevant transformation
    functions (and aliases) are mirroring those found in {!module:Scad} are
    provided. This allows for points in space represented by this type to moved
    around in a similar fashion to {!Scad.t}. *)
module Vec3 = struct
  include Vec3

  (** [multmatrix m t]

      Apply affine transformation matrix [m] to the vector [t]. *)
  let multmatrix = MultMatrix.transform

  (** [quaternion q t]

      Rotate [t] with the quaternion [q]. *)
  let quaternion = Quaternion.rotate_vec3

  (** [quaternion_about_pt q p t]

      Translates [t] along the vector [p], rotating the resulting vector with
      the quaternion [q], and finally, moving back along the vector [p].
      Functionally, rotating about the point in space arrived at by the initial
      translation along the vector [p]. *)
  let quaternion_about_pt = Quaternion.rotate_vec3_about_pt
end
