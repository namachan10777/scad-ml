module Scad = Scad
module Text = Text
module Color = Color
module MultMatrix = MultMatrix
module RotMatrix = RotMatrix
module Quaternion = Quaternion

(** 3-dimensional vector type.

    In addition to basic math and vector operations, relevant transformation
    functions (and aliases) are mirroring those found in {!module:Scad} are
    provided. This allows for points in space represented by this type to moved
    around in a similar fashion to {!Scad.t}. *)
module Vec3 = struct
  include Vec3

  (** [quaternion q t] *)
  let quaternion = Quaternion.rotate_vec3

  (** [quaternion_about_pt q p t] *)
  let quaternion_about_pt = Quaternion.rotate_vec3_about_pt
end
