(** Generation and measurement of 3d bezier curve (and patch/surface) functions.
    Including {!of_path}, which produces a bezier spline function that passes
    through all points of the given path. *)

include Bezier.S with type vec := Vec3.t

(** {1 Basic Transfomations} *)

val translate : Vec3.t -> t -> t
val rotate : Vec3.t -> t -> t
val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
val quaternion : Quaternion.t -> t -> t
val quaternion_about_pt : Quaternion.t -> Vec3.t -> t -> t
val vector_rotate : Vec3.t -> float -> t -> t
val vector_rotate_about_pt : Vec3.t -> float -> Vec3.t -> t -> t
val multmatrix : MultMatrix.t -> t -> t
val scale : Vec3.t -> t -> t
val mirror : Vec3.t -> t -> t
