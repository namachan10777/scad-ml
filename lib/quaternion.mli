(** Provides quaternion rotation functions. *)

type t = float * float * float * float

val id : t
val make : float * float * float -> float -> t
val add : t -> t -> t
val sub : t -> t -> t
val add_scalar : t -> float -> t
val sub_scalar : t -> float -> t
val scalar_sub_quat : t -> float -> t
val mul : t -> t -> t
val mul_scalar : t -> float -> t
val div_scalar : t -> float -> t
val negate : t -> t
val norm : t -> float
val normalize : t -> t
val dot : t -> t -> float
val conj : t -> t
val distance : t -> t -> float
val of_rotmatrix : RotMatrix.t -> t
val to_multmatrix : t -> MultMatrix.t
val to_string : t -> string
val get_x : t -> float
val get_y : t -> float
val get_z : t -> float
val get_w : t -> float

(** [slerp a b step]

    Spherical linear interpotation. Adapted from
    {{:https://github.com/KieranWynn/pyquaternion} pyquaternion}. *)
val slerp : t -> t -> float -> t

val rotate_vec3 : t -> Vec3.t -> Vec3.t
val rotate_vec3_about_pt : t -> Vec3.t -> Vec3.t -> Vec3.t
val alignment : Vec3.t -> Vec3.t -> t
