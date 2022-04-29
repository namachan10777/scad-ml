include Path.S with type vec := Vec3.t

type bbox =
  { min : Vec3.t
  ; max : Vec3.t
  }

(** [of_tups ps]

    Create a 3d path from a list of xyz coordinate triples. *)
val of_tups : (float * float * float) list -> t

(** {1 Drawing arcs (along a plane)} *)

(** [arc ?rev ?fn ?plane ?wedge ~centre ~radius ~start a]

    *)
val arc
  :  ?rev:bool
  -> ?fn:int
  -> ?plane:Plane.t
  -> ?wedge:bool
  -> centre:Vec3.t
  -> radius:float
  -> start:float
  -> float
  -> t

(** [arc_about_centre ?rev ?fn ?dir ?wedge ~centre p1 p2]

    *)
val arc_about_centre
  :  ?rev:bool
  -> ?fn:int
  -> ?dir:[ `CW | `CCW ]
  -> ?wedge:bool
  -> centre:Vec3.t
  -> Vec3.t
  -> Vec3.t
  -> t

(** [arc_through ?rev ?fn  ?wedge p1 p2 p3]

    *)
val arc_through : ?rev:bool -> ?fn:int -> ?wedge:bool -> Vec3.t -> Vec3.t -> Vec3.t -> t

(** [helix ?fn ?fa ?fs ?left ~n_turns ~pitch ?r2 r1]

    *)
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

(** [scaler ~len scale]

    *)
val scaler : len:int -> Vec2.t -> int -> MultMatrix.t

(** [twister ~len angle]

    *)
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
   paths for example, though {!Mesh.helix_extrude} may be a better fit in that
   case), but fail in others. For instance, [euler] can generate an abrupt when
    the path tangent is exactly vertical.

   If provided, [scale] and [twist], specify scaling and rotation to be linearly
   applied to along the path, analogous to the parameters of the same names in
   {!Scad.linear_extrude}. *)
val to_transforms : ?euler:bool -> ?scale:Vec2.t -> ?twist:float -> t -> MultMatrix.t list

(** [normal t]

  Calculate the normal vector of the path [t]. *)
val normal : t -> Vec3.t

(** [centroid ?eps t]

    *)
val centroid : ?eps:float -> t -> Vec3.t

(** [area ?signed t]

  Calculate the area of the co-planar path (describing a polygon) [t]. If
  [signed] is [true], the signed area is returned. *)
val area : ?signed:bool -> t -> float

(** [coplanar ?eps t]

  Returns [true] if all points in [t] are coplanar, within the tolerance [eps].
  If there are fewer than 3 points, or the path is colinear, this returns [false]. *)
val coplanar : ?eps:float -> t -> bool

(** [bbox t]

    *)
val bbox : t -> bbox

(** {1 Roundovers}*)

include Rounding.S with type vec := Vec.v3

(** {1 2d-3d Conversion} *)

(** [to_plane t]

    *)
val to_plane : t -> Plane.t

(** [project plane t]

    *)
val project : Plane.t -> t -> Vec2.t list

(** [of_path2 ?plane path]

    *)
val of_path2 : ?plane:Plane.t -> Path2.t -> t

(** [to_path2 ?plane t]

    *)
val to_path2 : ?plane:Plane.t -> t -> Path2.t

(** {1 Basic Shapes} *)

val circle : ?fn:int -> ?plane:Plane.t -> float -> t
val square : ?center:bool -> ?plane:Plane.t -> Vec2.t -> t

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
