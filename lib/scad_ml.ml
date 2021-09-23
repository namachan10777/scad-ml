module Scad = Scad
module Text = Text
module Color = Color
module MultMatrix = MultMatrix
module RotMatrix = RotMatrix
module Quaternion = Quaternion

module Vec3 = struct
  include Vec3

  let quaternion = Quaternion.rotate_vec3
  let quaternion_about_pt = Quaternion.rotate_vec3_about_pt
end
