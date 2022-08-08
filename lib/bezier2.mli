(** Generation and measurement of 2d bezier curve (and patch/surface) functions.
    Including {!of_path}, which produces a bezier spline function that passes
    through all points of the given path. *)

include Bezier.S with type vec := Vec2.t (** @inline *)

(** {1 2d specific functionality} *)

(** [line_intersection ~line pts]

    Compute the positions (between [0.] and [1.]) along the bezier curve defined
    by the control points [pts] that [line] intersects with. *)
val line_intersection : line:Vec2.line -> Vec2.t list -> float list

(** {1 Basic Transfomations} *)

val translate : Vec2.t -> t -> t
val rotate : ?about:Vec2.t -> float -> t -> t
val zrot : ?about:Vec2.t -> float -> t -> t
val scale : Vec2.t -> t -> t
val mirror : Vec2.t -> t -> t
val affine : Affine2.t -> t -> t
val affine3 : Affine3.t -> t -> float -> Vec3.t
val quaternion : ?about:Vec3.t -> Quaternion.t -> t -> float -> Vec3.t
val axis_rotate : ?about:Vec3.t -> Vec3.t -> float -> t -> float -> Vec3.t
