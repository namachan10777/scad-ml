(** Generation and measurement of 2d bezier curve (and patch/surface) functions.
    Including {!of_path}, which produces a bezier spline function that passes
    through all points of the given path. *)

include Bezier.S with type vec := Vec2.t

(** {1 2d specific functionality} *)

(** [line_intersection ~line pts]

    Compute the positions (between [0.] and [1.]) along the bezier curve defined
    by the control points [pts] that [line] intersects with. *)
val line_intersection : line:Vec2.line -> Vec2.t list -> float list
