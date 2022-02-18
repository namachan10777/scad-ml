include Path.S with type vec := Vec3.t

val arc
  :  ?init:Vec3.t list
  -> ?rev:bool
  -> ?fn:int
  -> centre:Vec3.t
  -> radius:float
  -> start:float
  -> float
  -> Vec3.t list

val arc_about_centre
  :  ?init:Vec3.t list
  -> ?rev:bool
  -> ?fn:int
  -> ?dir:[ `CW | `CCW ]
  -> centre:Vec3.t
  -> Vec3.t
  -> Vec3.t
  -> Vec3.t list

val arc_through : ?init:t -> ?rev:bool -> ?fn:int -> Vec3.t -> Vec3.t -> Vec3.t -> t

val helix
  :  ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?left:bool
  -> n_turns:int
  -> pitch:float
  -> ?r2:float
  -> float
  -> t

val scaler : len:int -> float * float -> int -> MultMatrix.t
val twister : len:int -> float -> int -> MultMatrix.t

(** [to_transforms t]

   Generate list of transformations that can be applied to three-dimensional
   vectors ({!Vec3.t} via {!MultMatrix.transform}) or shapes ({!Scad.d3} via
   {!Scad.multmatrix}), to move them along the path [t] (intended to be applied
   to the vector/shape from its original position each time).

   Tangents are used to estimate appropriate rotations for each translation,
   using quaternion alignment from tangent to tangent, accumulating rotation
   along the way by default, when [euler = false]. Some effort is made in this
   mode to vector of the shape being swept consistent and sensible, though some
   rotation of the shape before sweeping may be necessary to get the desired
   result. Setting [euler = true] will use euler rotations instead, which can
   have results more in line with expectations in some scenarios (helical-like
   paths for example, though {!Poly3d.helix_extrude} may be a better fit in that
   case), but fail in others. For instance, [euler] can generate an abrupt when
    the path tangent is exactly vertical.

   If provided, [scale] and [twist], specify scaling and rotation to be linearly
   applied to along the path, analogous to the parameters of the same names in
   {!Scad.linear_extrude}. *)
val to_transforms : ?euler:bool -> ?scale:Vec2.t -> ?twist:float -> t -> MultMatrix.t list

(** [normal t]

  Calculate the normal vector of the path [t]. *)
val normal : t -> Vec3.t

(** [coplanar ?eps t]

  Returns [true] if all points in [t] are coplanar, within the tolerance [eps].
  If there are fewer than 3 points, or the path is colinear, this returns [false]. *)
val coplanar : ?eps:float -> t -> bool

val to_plane : t -> Plane.t

(** {1 Basic Transfomations} *)

val translate : Vec3.t -> t -> t
val rotate : Vec3.t -> t -> t
val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
val quaternion : Quaternion.t -> t -> t
val quaternion_about_pt : Quaternion.t -> Vec3.t -> t -> t
val vector_rotate : Vec3.t -> float -> t -> t
val vector_rotate_about_pt : Vec3.t -> float -> Vec3.t -> t -> t
val multmatrix : MultMatrix.t -> t -> t
val scale : Vec3.t -> t -> t
val mirror : Vec3.t -> t -> t
