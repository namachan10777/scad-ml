(** Generation and measurement of 2d bezier curve (and patch/surface) functions.
    Including {!of_path}, which produces a bezier spline function that passes
    through all points of the given path. *)

include Bezier.S with type vec := Vec2.t

(** {1 2d specific functionality} *)

(** [line_intersection ~line pts]

    Compute the positions (between [0.] and [1.]) along the bezier curve defined
    by the control points [pts] that [line] intersects with. *)
val line_intersection : line:Vec2.line -> Vec2.t list -> float list

(** {1 Basic Transfomations} *)

val translate : Vec2.t -> t -> t
val rotate : float -> t -> t
val rotate_about_pt : float -> Vec2.t -> t -> t
val scale : Vec2.t -> t -> t
val mirror : Vec2.t -> t -> t
val multmatrix : MultMatrix.t -> t -> float -> Vec3.t
val quaternion : Quaternion.t -> t -> float -> Vec3.t
val quaternion_about_pt : Quaternion.t -> Vec3.t -> t -> float -> Vec3.t
val vector_rotate : Vec3.t -> float -> t -> float -> Vec3.t
val vector_rotate_about_pt : Vec3.t -> float -> Vec3.t -> t -> float -> Vec3.t
