(** Generation and measurement of 3d bezier curve (and patch/surface) functions.
    Including {!of_path}, which produces a bezier spline function that passes
    through all points of the given path. *)

include Bezier.S with type vec := Vec3.t (** @inline *)

(** {1 Basic Transfomations} *)

val translate : Vec3.t -> t -> t
val xtrans : float -> t -> t
val ytrans : float -> t -> t
val ztrans : float -> t -> t
val rotate : ?about:Vec3.t -> Vec3.t -> t -> t
val xrot : ?about:Vec3.t -> float -> t -> t
val yrot : ?about:Vec3.t -> float -> t -> t
val zrot : ?about:Vec3.t -> float -> t -> t
val quaternion : ?about:Vec3.t -> Quaternion.t -> t -> t
val axis_rotate : ?about:Vec3.t -> Vec3.t -> float -> t -> t
val affine : Affine3.t -> t -> t
val scale : Vec3.t -> t -> t
val mirror : Vec3.t -> t -> t
