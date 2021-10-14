(** Provides functions for the creation of and operations between
    {{:https://en.wikipedia.org/wiki/Quaternion} quaternions}. These can be used
    to create composable and interpolatable rotations to be applied to vectors
    (e.g. {!Vec3.t}) directly, and {!Scad.t} through {!MultMatrix.t}. *)

type t = float * float * float * float

(** The identity quaternion: [(0., 0., 0., 1.)] *)
val id : t

(** [make ax angle]

    Create a quaternion representing a rotation of [angle] (in radians) around
    the vector [ax]. *)
val make : Vec3.t -> float -> t

(** {1 Basic Arithmetic} *)

(** [add a b]

    Hadamard (element-wise) addition of quaternions [a] and [b]. *)
val add : t -> t -> t

(** [sub a b]

    Hadamard (element-wise) subtraction of quaternion [b] from [a]. *)
val sub : t -> t -> t

(** [mul a b]

    Quaternion multiplication of [a] and [b]. *)
val mul : t -> t -> t

(** [negate t]

    Negation of all elements of [t]. *)
val negate : t -> t

(** [add_scalar t s]

    Add [s] to the magnitude of [t], leaving the imaginary parts unchanged. *)
val add_scalar : t -> float -> t

(** [sub_scalar t s]

    Subtract [s] from the magnitude of [t], leaving the imaginary parts
    unchanged. *)
val sub_scalar : t -> float -> t

(** [scalar_sub_quat t s]

    Negate the imaginary parts of [t], and subtract the magnitude from [s] to
    obtain the new magnitude. *)
val scalar_sub_quat : t -> float -> t

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

(** [normalize t]

    Normalize [t] to a quaternion for which the magnitude is equal to 1.
    e.g. [norm (normalize t) = 1.] *)
val normalize : t -> t

(** [dot a b]

    Vector dot product of [a] and [b]. *)
val dot : t -> t -> float

(** [conj t]

    Take the conjugate of the quaternion [t], negating the imaginary parts (x,
    y, and z) of [t], leaving the magnitude unchanged. *)
val conj : t -> t

(** [distance a b]

    Calculate the magnitude of the difference (Hadamard subtraction) between [a]
    and [b]. *)
val distance : t -> t -> float

(** {1 Matrix Conversions} *)

val of_rotmatrix : RotMatrix.t -> t
val to_multmatrix : t -> MultMatrix.t

(** {1 Utilities} *)

val to_string : t -> string
val get_x : t -> float
val get_y : t -> float
val get_z : t -> float
val get_w : t -> float

(** [slerp a b step]

    Spherical linear interpotation. Adapted from
    {{:https://github.com/KieranWynn/pyquaternion} pyquaternion}. *)
val slerp : t -> t -> float -> t

(** {1 Vector Transformations} *)

(** [rotate_vec3 t v]

    Rotate [v] with the quaternion [t]. *)
val rotate_vec3 : t -> Vec3.t -> Vec3.t

(** [rotate_vec3_about_pt t p v]

    Translates [v] along the vector [p], rotating the resulting vector with the
    quaternion [t], and finally, moving back along the vector [p]. Functionally,
    rotating about the point in space arrived at by the initial translation
    along the vector [p]. *)
val rotate_vec3_about_pt : t -> Vec3.t -> Vec3.t -> Vec3.t

(** [alignment a b]

    Calculate a quaternion that would bring [a] into alignment with [b]. *)
val alignment : Vec3.t -> Vec3.t -> t
