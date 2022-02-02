type t = Vec2.t list

(** [lift_plane plane p]

 Lift the 2d point [p] onto the provided [plane] described by 3 points, the
 first of which represents the origin [(0., 0.)] of the target 2d coordinate
 system, and the second being a point that falls along the positive y-axis.
 Throws [Invalid_argument] if the points of [plane] are colinear. *)
val lift_plane : Vec3.t * Vec3.t * Vec3.t -> float * float -> Vec3.t

val total_travel' : Vec2.t array -> float
val total_travel : t -> float
val cummulative_travel : t -> float list
val to_continuous : t -> float -> Vec2.t
val resample : freq:[< `N of int | `Spacing of float ] -> t -> t
val arc_through : ?init:t -> ?rev:bool -> ?fn:int -> Vec2.t -> Vec2.t -> Vec2.t -> t
val translate : Vec2.t -> t -> t
val rotate : float -> t -> t
val rotate_about_pt : float -> Vec2.t -> t -> t
val scale : Vec2.t -> t -> t
val mirror : Vec2.t -> t -> t
