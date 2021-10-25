(** Provides functions for the creation of and operations between
    {{:https://en.wikipedia.org/wiki/Quaternion} quaternions}. These can be used
    to create composable and interpolatable rotations to be applied to vectors
    (e.g. {!Vec3.t}) directly, and {!Scad.t} through {!MultMatrix.t}. *)

type t = float * float * float * float

val id : t
(** The identity quaternion: [(0., 0., 0., 1.)] *)

val make : Vec3.t -> float -> t
(** [make ax angle]

    Create a quaternion representing a rotation of [angle] (in radians) around
    the vector [ax]. *)

(** {1 Basic Arithmetic} *)

val add : t -> t -> t
(** [add a b]

    Hadamard (element-wise) addition of quaternions [a] and [b]. *)

val sub : t -> t -> t
(** [sub a b]

    Hadamard (element-wise) subtraction of quaternion [b] from [a]. *)

val mul : t -> t -> t
(** [mul a b]

    Quaternion multiplication of [a] and [b]. *)

val negate : t -> t
(** [negate t]

    Negation of all elements of [t]. *)

val add_scalar : t -> float -> t
(** [add_scalar t s]

    Add [s] to the magnitude of [t], leaving the imaginary parts unchanged. *)

val sub_scalar : t -> float -> t
(** [sub_scalar t s]

    Subtract [s] from the magnitude of [t], leaving the imaginary parts
    unchanged. *)

val scalar_sub_quat : t -> float -> t
(** [scalar_sub_quat t s]

    Negate the imaginary parts of [t], and subtract the magnitude from [s] to
    obtain the new magnitude. *)

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

val normalize : t -> t
(** [normalize t]

    Normalize [t] to a quaternion for which the magnitude is equal to 1. e.g.
    [norm (normalize t) = 1.] *)

val dot : t -> t -> float
(** [dot a b]

    Vector dot product of [a] and [b]. *)

val conj : t -> t
(** [conj t]

    Take the conjugate of the quaternion [t], negating the imaginary parts (x,
    y, and z) of [t], leaving the magnitude unchanged. *)

val distance : t -> t -> float
(** [distance a b]

    Calculate the magnitude of the difference (Hadamard subtraction) between [a]
    and [b]. *)

(** {1 Matrix Conversions} *)

val of_rotmatrix : RotMatrix.t -> t
val to_multmatrix : t -> MultMatrix.t

(** {1 Utilities} *)

val to_string : t -> string
val get_x : t -> float
val get_y : t -> float
val get_z : t -> float
val get_w : t -> float

val slerp : t -> t -> float -> t
(** [slerp a b step]

    Spherical linear interpotation. Adapted from
    {{:https://github.com/KieranWynn/pyquaternion} pyquaternion}. *)

(** {1 Vector Transformations} *)

val rotate_vec3 : t -> Vec3.t -> Vec3.t
(** [rotate_vec3 t v]

    Rotate [v] with the quaternion [t]. *)

val rotate_vec3_about_pt : t -> Vec3.t -> Vec3.t -> Vec3.t
(** [rotate_vec3_about_pt t p v]

    Translates [v] along the vector [p], rotating the resulting vector with the
    quaternion [t], and finally, moving back along the vector [p]. Functionally,
    rotating about the point in space arrived at by the initial translation
    along the vector [p]. *)

val alignment : Vec3.t -> Vec3.t -> t
(** [alignment a b]

    Calculate a quaternion that would bring [a] into alignment with [b]. *)
