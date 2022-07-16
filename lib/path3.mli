(** 3d path generation (including arcs and basic shapes), manipulation
   (including roundovers (see {!module:Round}), and conversion to sweeping
   transformations with {!to_transforms}), and measurement. *)

include Path.S with type vec := Vec3.t and type line := Vec3.line

(** [nearby_idxs ?min_tree_size ?radius path p]

    Find the indices of points within [radius] (default = [1e-9]) distance from
    the target point [p] in [path]. Match indices will be returned in arbitrary
    order (unsorted). When [path] is provided (eagerly on partial application),
    the length will be checked and a function to perform the search will be
    generated. If [path] is shorter than [min_tree_size], it will be a simple
    direct search otherwise a {!BallTree3.t} will be constructed. Thus, if you
    plan to search for more than one target point, take care to apply this
    function in two steps to avoid repeated length checks and closure/tree
    generations. *)
val nearby_idxs : ?min_tree_size:int -> ?radius:float -> Vec3.t list -> Vec3.t -> int list

(** [nearby_points ?min_tree_size ?radius path]

    Find the points within [radius] (default = [1e-9]) distance from the target
    point [p] in [path]. Matched points will be returned in arbitrary order
    (unsorted). When [path] is provided (eagerly on partial application), the
    length will be checked and a function to perform the search will be
    generated. If [path] is shorter than [min_tree_size], it will be a simple
    direct search otherwise a {!BallTree3.t} will be constructed. Thus, if you
    plan to search for more than one target point, take care to apply this
    function in two steps to avoid repeated length checks and closure/tree
    generations. *)
val nearby_points
  :  ?min_tree_size:int
  -> ?radius:float
  -> Vec3.t list
  -> Vec3.t
  -> Vec3.t list

(** {1 Creation and 2d-3d Conversion} *)

(** [of_tups ps]

    Create a 3d path from a list of xyz coordinate triples. *)
val of_tups : (float * float * float) list -> t

(** [of_path2 ?plane path]

    Lift a 2d [path] onto [plane] (default = {!Plane.xy}). *)
val of_path2 : ?plane:Plane.t -> Path2.t -> t

(** [to_path2 ?plane t]

    Project the 3d path [t] onto [plane] (default = {!Plane.xy}). *)
val to_path2 : ?plane:Plane.t -> t -> Path2.t

(** [to_plane ?eps t]

    Compute the normalized cartesian equation of the plane that the path [t]
   resides on. If there are fewer than three points in [t], or they are not
   coplanar within the tolerance [eps], an [Invalid_argument] exception is
   raised. *)
val to_plane : ?eps:float -> t -> Plane.t

(** [project plane t]

    Project the 3d path [t] onto [plane]. *)
val project : Plane.t -> t -> Path2.t

(** {1 Basic Shapes} *)

(** [circle ?fn ?fa ?fs ?plane radius]

    Draw a circular path of radius [r] onto [plane] (default = {!Plane.xy}). *)
val circle : ?fn:int -> ?fa:float -> ?fs:float -> ?plane:Plane.t -> float -> t

(** [square ?center ?plane dims]

    Draw a rectangular path with xy [dims] (e.g. width and height) onto
    [plane] (default = {!Plane.xy}). If [center] is [true] then the path will be
    centred around the origin (default = [false]). *)
val square : ?center:bool -> ?plane:Plane.t -> Vec2.t -> t

(** [ellipse ?fn ?fa ?fs ?plane radii]

    Draw an ellipse with xy [radii] onto [plane] (default = {!Plane.xy}). The
    greater of the two radii is used for fragment/resolution calculation. *)
val ellipse : ?fn:int -> ?fa:float -> ?fs:float -> ?plane:Plane.t -> Vec2.t -> t

(** [star ?plane ~r1 ~r2 n]

    Draw an [n] pointed star with inner radius [r1] and outer radius [r2] onto
    [plane] (default = {!Plane.xy}). *)
val star : ?plane:Plane.t -> r1:float -> r2:float -> int -> t

(** {1 Drawing Arcs} *)

(** [arc ?rev ?fn ?fa ?fs ?plane ?wedge ~centre ~radius ~start a]

    Draw an arc onto a 3d [plane] (default = {!Plane.xy}). See {!Path2.arc}. *)
val arc
  :  ?rev:bool
  -> ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?plane:Plane.t
  -> ?wedge:bool
  -> centre:Vec3.t
  -> radius:float
  -> start:float
  -> float
  -> t

(** [arc_about_centre ?rev ?fn ?fa ?fs ?dir ?wedge ~centre p1 p2]

    Draw an arc between [p1] and [p2], about [centre], on the 3d plane occupied
    by the three points. See {!Path2.arc_about_centre}. *)
val arc_about_centre
  :  ?rev:bool
  -> ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?dir:[ `CW | `CCW ]
  -> ?wedge:bool
  -> centre:Vec3.t
  -> Vec3.t
  -> Vec3.t
  -> t

(** [arc_through ?rev ?fn ?fa ?fs ?wedge p1 p2 p3]

    Draw an arc through the points [p1], [p2], and [p3]. See {!Path2.arc_through}. *)
val arc_through
  :  ?rev:bool
  -> ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?wedge:bool
  -> Vec3.t
  -> Vec3.t
  -> Vec3.t
  -> t

(** [helix ?fn ?fa ?fs ?left ~n_turns ~pitch ?r2 r1]

    Draw a 3d helical path around a cylinder/cone with start radius [r1] and
    end radius [r2] (default = [r1]).
    - [n_turns] sets the number of revolutions around the z-axis
    - [pitch] describes the height of one complete turn
    - [left] is used to set handedness (default = [true])
    - [fn], [fa], and [fs] parameters govern quality as they do in OpenSCAD *)
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

(** {1 Roundovers}

    Specification and application of circular, chamfer, and bezier (continuous
    curvature) roundovers to 3d paths.

    Based on the {{:https://github.com/revarbat/BOSL2} BOSL2}
    {{:https://github.com/revarbat/BOSL2/blob/master/rounding.scad} rounding}
    module. *)

include Rounding.S with type vec := Vec3.t

(** {1 Geometry} *)

(** [normal t]

   Calculate the normal vector of the path [t]. An [Invalid_argument] exception
   is raised if there are fewer than three points in [t]. *)
val normal : t -> Vec3.t

(** [centroid ?eps t]

    Compute the centroid of the path [t]. If [t] is collinear or
   self-intersecting (within [eps] tolerance), an [Invalid_argument] exception
   is raised. *)
val centroid : ?eps:float -> t -> Vec3.t

(** [area ?signed t]

  Calculate the area of the co-planar path (describing a polygon) [t]. If
  [signed] is [true], the signed area is returned. *)
val area : ?signed:bool -> t -> float

(** [coplanar ?eps t]

  Returns [true] if all points in [t] are coplanar, within the tolerance [eps].
  If there are fewer than 3 points, or the path is collinear, this returns [false]. *)
val coplanar : ?eps:float -> t -> bool

(** [bbox t]

    Compute the 3d bounding box of the path [t]. *)
val bbox : t -> Vec3.bbox

(** {1 Sweeping Transform Helpers} *)

(** [scaler ~len scale]

    Create a lookup from index to scaling transformation matrix for
   interpolating from [{x = 1.; y = 1.}] at [0] to [scale] by [len - 1]. *)
val scaler : len:int -> Vec2.t -> int -> MultMatrix.t

(** [twister ~len angle]

    Create a lookup from index to rotation transformation matrix for
    interpolating from [0.] (no rotation) at [0] to [angle] by [len - 1]. *)
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

(** {1 Debugging helpers} *)

val show_points : (int -> Scad.d3) -> t -> Scad.d3
