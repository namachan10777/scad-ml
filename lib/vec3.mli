type t = float * float * float

val zero : t
(** Zero vector = [(0., 0., 0.)] *)

val equal : t -> t -> bool

(** {1 Basic Arithmetic} *)

val horizontal_op : (float -> float -> float) -> t -> t -> t
(** [horizontal_op f a b]

    Hadamard (element-wise) operation between vectors [a] and [b] using the
    function [f]. *)

val add : t -> t -> t
(** [add a b]

    Hadamard (element-wise) addition of vectors [a] and [b]. *)

val sub : t -> t -> t
(** [sub a b]

    Hadamard (element-wise) subtraction of vector [b] from [a]. *)

val mul : t -> t -> t
(** [mul a b]

    Hadamard (element-wise) product of vectors [a] and [b]. *)

val div : t -> t -> t
(** [div a b]

    Hadamard (element-wise) division of vector [a] by [b]. *)

val negate : t -> t
(** [negate t]

    Negation of all elements of [t]. *)

val add_scalar : t -> float -> t
(** [add_scalar t s]

    Element-wise addition of [s] to [t]. *)

val sub_scalar : t -> float -> t
(** [sub_scalar t s]

    Element-wise subtraction of [s] from [t]. *)

val mul_scalar : t -> float -> t
(** [mul_scalar t s]

    Element-wise multiplication of [t] by [s]. *)

val div_scalar : t -> float -> t
(** [div_scalar t s]

    Element-wise division of [t] by [s]. *)

(** {1 Vector Math} *)

val norm : t -> float
(** [norm t]

    Calculate the vector norm (a.k.a. magnitude) of [t]. *)

val distance : t -> t -> float
(** [distance a b]

    Calculate the magnitude of the difference (Hadamard subtraction) between [a]
    and [b]. *)

val normalize : t -> t
(** [normalize t]

    Normalize [t] to a vector for which the magnitude is equal to 1. e.g.
    [norm (normalize t) = 1.] *)

val dot : t -> t -> float
(** [dot a b]

    Vector dot product of [a] and [b]. *)

val cross : t -> t -> t
(** [cross a b]

    Vector cross product of [a] and [b]. *)

val mean : t list -> t
(** [mean l]

    Calculate the mean / average of all vectors in [l]. *)

(** {1 Transformations}

    Equivalent to those found in {!module:Scad}. Quaternion operations are
    provided when this module is included in {!module:Scad_ml}. *)

val rotate_x : float -> t -> t
(** [rotate_x theta t]

    Rotate [t] by [theta] radians about the x-axis. *)

val rotate_y : float -> t -> t
(** [rotate_y theta t]

    Rotate [t] by [theta] radians about the y-ayis. *)

val rotate_z : float -> t -> t
(** [rotate_z theta t]

    Rotate [t] by [theta] radians about the z-azis. *)

val rotate : t -> t -> t
(** [rotate r t]

    Euler (xyz) rotation of [t] by the angles in [theta]. Equivalent to
    [rotate_x rx t |> rotate_y ry |> rotate_z rz], where [(rx, ry, rz) = r]. *)

val rotate_about_pt : t -> t -> t -> t
(** [rotate_about_pt r pivot t]

    Translates [t] along the vector [pivot], euler rotating the resulting vector
    with [r], and finally, moving back along the vector [pivot]. Functionally,
    rotating about the point in space arrived at by the initial translation
    along the vector [pivot]. *)

val translate : t -> t -> t
(** [translate p t]

    Translate [t] along the vector [p]. Equivalent to {!val:add}. *)

val scale : t -> t -> t
(** [scale s t]

    Scale [t] by factors [s]. Equivalent to {!val:mul}. *)

val mirror : t -> t -> t
(** [mirror ax t]

    Mirrors [t] on a plane through the origin, defined by the normal vector
    [ax]. *)

val projection : t -> t
(** [projection t]

    Project [t] onto the XY plane. *)

(** {1 Utilities} *)

val map : (float -> 'b) -> t -> 'b * 'b * 'b
val get_x : t -> float
val get_y : t -> float
val get_z : t -> float
val to_string : t -> string

val deg_of_rad : t -> t
(** [deg_of_rad t]

    Element-wise conversion of [t] from radians to degrees. *)

val rad_of_deg : t -> t
(** [rad_to_deg t]

    Element-wise conversion of [t] from degrees to radians. *)

(** {1 2d - 3d conversion} *)

val to_vec2 : t -> float * float
val of_vec2 : float * float -> t

(** {1 Infix operations} *)

val ( <+> ) : t -> t -> t
(** [a <+> b]

    Hadamard (element-wise) addition of [a] and [b]. *)

val ( <-> ) : t -> t -> t
(** [a <-> b]

    Hadamard (element-wise) subtraction of [b] from [a]. *)

val ( <*> ) : t -> t -> t
(** [a <*> b]

    Hadamard (element-wise) product of [a] and [b]. *)

val ( </> ) : t -> t -> t
(** [a </> b]

    Hadamard (element-wise) division of [a] by [b]. *)
