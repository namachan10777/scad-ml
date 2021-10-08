(** Provides quaternion rotation functions. *)

type t = float * float * float * float

(** The identity quaternion *)
val id : t

(** [make ax angle]

    Create a quaternion representing a rotation of [angle] (in radians) around
    the vector [ax]. *)
val make : Vec3.t -> float -> t

(** {1 Basic Arithmetic} *)

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val negate : t -> t
val add_scalar : t -> float -> t
val sub_scalar : t -> float -> t
val scalar_sub_quat : t -> float -> t
val mul_scalar : t -> float -> t
val div_scalar : t -> float -> t

(** {1 Vector Math} *)

val norm : t -> float
val normalize : t -> t
val dot : t -> t -> float

(** [conj t]

    Take the conjugate of the quaternion [t], negating the imaginary parts (x,
    y, and z) of [t], leaving the magnitude unchanged. *)
val conj : t -> t

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

val rotate_vec3 : t -> Vec3.t -> Vec3.t
val rotate_vec3_about_pt : t -> Vec3.t -> Vec3.t -> Vec3.t

(** [alignment a b]

    Calculate a quaternion that would bring [a] into alignment with [b]. *)
val alignment : Vec3.t -> Vec3.t -> t
