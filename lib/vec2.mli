type t = float * float

(** Zero vector = [(0., 0.)] *)
val zero : t

val equal : t -> t -> bool

(** {1 Basic Arithmetic} *)

(** [horizontal_op f a b]

    Hadamard (element-wise) operation between vectors [a] and [b] using the
    function [f]. *)
val horizontal_op : (float -> float -> float) -> t -> t -> t

(** [add a b]

    Hadamard (element-wise) addition of vectors [a] and [b]. *)
val add : t -> t -> t

(** [sub a b]

    Hadamard (element-wise) subtraction of vector [b] from [a]. *)
val sub : t -> t -> t

(** [mul a b]

    Hadamard (element-wise) product of vectors [a] and [b]. *)
val mul : t -> t -> t

(** [div a b]

    Hadamard (element-wise) division of vector [a] by [b]. *)
val div : t -> t -> t

(** [negate t]

    Negation of all elements of [t]. *)
val negate : t -> t

(** [add_scalar t s]

    Element-wise addition of [s] to [t]. *)
val add_scalar : t -> float -> t

(** [sub_scalar t s]

    Element-wise subtraction of [s] from [t]. *)
val sub_scalar : t -> float -> t

(** [mul_scalar t s]

    Element-wise multiplication of [t] by [s]. *)
val mul_scalar : t -> float -> t

(** [div_scalar t s]

    Element-wise division of [t] by [s]. *)
val div_scalar : t -> float -> t

(** {1 Vector Math} *)

(** [norm t]

    Calculate the vector norm (a.k.a. magnitude) of [t]. *)
val norm : t -> float

(** [distance a b]

    Calculate the magnitude of the difference (Hadamard subtraction) between [a]
    and [b]. *)
val distance : t -> t -> float

(** [normalize t]

    Normalize [t] to a vector for which the magnitude is equal to 1.
    e.g. [norm (normalize t) = 1.] *)
val normalize : t -> t

(** [dot a b]

    Vector dot product of [a] and [b]. *)
val dot : t -> t -> float

(** [mean l]

    Calculate the mean / average of all vectors in [l]. *)
val mean : t list -> t

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

(** {1 Utilities} *)

val map : (float -> 'b) -> t -> 'b * 'b
val get_x : t -> float
val get_y : t -> float
val to_string : t -> string

(** [deg_of_rad t]

    Element-wise conversion of [t] from radians to degrees. *)
val deg_of_rad : t -> t

(** [rad_to_deg t]

    Element-wise conversion of [t] from degrees to radians. *)
val rad_of_deg : t -> t

(** {1 2d - 3d conversion} *)

val of_vec3 : float * float * float -> t
val to_vec3 : t -> float * float * float

(** {1 Infix operations} *)

(** [a <+> b]

    Hadamard (element-wise) addition of [a] and [b]. *)
val ( <+> ) : t -> t -> t

(** [a <-> b]

    Hadamard (element-wise) subtraction of [b] from [a]. *)
val ( <-> ) : t -> t -> t

(** [a <*> b]

    Hadamard (element-wise) product of [a] and [b]. *)
val ( <*> ) : t -> t -> t

(** [a </> b]

    Hadamard (element-wise) division of [a] by [b]. *)
val ( </> ) : t -> t -> t
