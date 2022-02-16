include Sigs.Vec with type t = float * float * float

(** {1 Transformations}

    Equivalent to those found in {!module:Scad}. Quaternion operations are
    provided when this module is included in {!module:Scad_ml}. *)

(** [rotate_x theta t]

    Rotate [t] by [theta] radians about the x-axis. *)
val rotate_x : float -> t -> t

(** [rotate_y theta t]

    Rotate [t] by [theta] radians about the y-ayis. *)
val rotate_y : float -> t -> t

(** [rotate_z theta t]

    Rotate [t] by [theta] radians about the z-azis. *)
val rotate_z : float -> t -> t

(** [rotate r t]

    Euler (xyz) rotation of [t] by the angles in [theta]. Equivalent to
    [rotate_x rx t |> rotate_y ry |> rotate_z rz], where [(rx, ry, rz) = r]. *)
val rotate : t -> t -> t

(** [rotate_about_pt r pivot t]

    Translates [t] along the vector [pivot], euler rotating the resulting vector
    with [r], and finally, moving back along the vector [pivot]. Functionally,
    rotating about the point in space arrived at by the initial translation
    along the vector [pivot]. *)
val rotate_about_pt : t -> t -> t -> t

(** [translate p t]

    Translate [t] along the vector [p]. Equivalent to {!val:add}. *)
val translate : t -> t -> t

(** [scale s t]

    Scale [t] by factors [s]. Equivalent to {!val:mul}. *)
val scale : t -> t -> t

(** [mirror ax t]

    Mirrors [t] on a plane through the origin, defined by the normal vector
    [ax]. *)
val mirror : t -> t -> t

(** [projection t]

    Project [t] onto the XY plane. *)
val projection : t -> t

(** {1 2d - 3d conversion} *)

val to_vec2 : t -> float * float
val of_vec2 : ?z:float -> float * float -> t

(** [project_plane plane p]

 Project the 3d point [p] onto the provided [plane] described by 3 points, the
 first of which represents the origin [(0., 0.)] of the target 2d coordinate
 system, and the second being a point that falls along the positive y-axis.
 The final point being a point co-planar with the first two. See
 {!Path2d.lift_plane} for a means to lift points back out of 2d onto the same
 plane. Throws [Invalid_argument] if the points of [plane] are colinear. *)
val project_plane : t * t * t -> t -> float * float

(** [lift_plane plane p]

 Lift the 2d point [p] onto the provided [plane] described by 3 points, the
 first of which represents the origin [(0., 0.)] of the target 2d coordinate
 system, and the second being a point that falls along the positive y-axis.
 Throws [Invalid_argument] if the points of [plane] are colinear. *)
val lift_plane : t * t * t -> float * float -> t
