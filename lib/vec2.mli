include Sigs.Vec with type t = float * float

(** {1 Transformations}

    Equivalent to those found in {!module:Scad}. Quaternion operations are
    provided when this module is included in {!module:Scad_ml}. *)

(** [rotate r t]

    Rotation of [t] about the origin by [r] (in radians). *)
val rotate : float -> t -> t

(** [rotate_about_pt r pivot t]

    Translates [t] along the vector [pivot], rotating the resulting vector
    with [r], and finally, moving back along the vector [pivot]. Functionally,
    rotating about the point in space arrived at by the initial translation
    along the vector [pivot]. *)
val rotate_about_pt : float -> t -> t -> t

(** [translate p t]

    Translate [t] along the vector [p]. Equivalent to {!val:add}. *)
val translate : t -> t -> t

(** [scale s t]

    Scale [t] by factors [s]. Equivalent to {!val:mul}. *)
val scale : t -> t -> t

(** [mirror ax t]

    Mirrors [t] on a plane through the origin, defined by the normal vector [ax]. *)
val mirror : t -> t -> t

(** {1 2d - 3d conversion} *)

val of_vec3 : float * float * float -> t
val to_vec3 : ?z:float -> t -> float * float * float
