type t = Mesh0.t = private
  { n_points : int
  ; points : Vec3.t list
  ; faces : int list list
  }

type row_wrap =
  [ `Loop
  | `Both
  | `None
  | `Top
  | `Bot
  ]

(** [empty]

    An empty [t], with no points. *)
val empty : t

(** [make ~points ~faces]

    Create a mesh [t] from a list of {!Vec3.t} [points], and a list of [faces]
    described by indices into [points]. *)
val make : points:Vec3.t list -> faces:int list list -> t

(** [of_rows ?row_wrap ?col_wrap rows]

    Create a {!type:t} representing a polyhedron from a list of layers
    (counter_clockwise loops of 3d points). [row_wrap] defaults to [`Both], which
    specifies that faces should be generated to close off the bottom and top
   layers of the generated shape. If it is instead set to [`Loop], the open
   faces of the first and last layers will be closed with one another. For more
   advanced usages, one or both of the caps can be left open, so the resulting
   meshes can be closed off by some other means. [col_wrap] sets whether faces
    should be generated to loop between the ends of each row. If [rows] is empty, a
   {!empty} is returned.  Throws [Invalid_argument] if [rows] contains only
   one row, or if it is not rectangular (any row differs in length). *)
val of_rows : ?row_wrap:row_wrap -> ?col_wrap:bool -> Vec3.t list list -> t

(** [of_ragged ?looped ?reverse rows]

    Create a triangular mesh from a list of rows, where each row can differ in
    length relative to its neighbours by up to 2. Since the rows can be ragged,
    no (columnar) wrapping is done, thus they are best described as rows, rather
    than layers as with {!of_rows} which produces an enclosed polyhedron.
    Instead, this function is useful for the generation of triangular patches
    that can be joined with one another to create a complete polyhedron. Setting
    [looped] to true will generate faces between the last and first rows, so long
    as their lengths differ by no more than 2. Face winding order is reversed if
    [reverse] is [true]. Throws [Invalid_argument] if a row length delta of
    greater than 2 is encountered. *)
val of_ragged : ?looped:bool -> ?rev:bool -> Vec3.t list list -> t

(** [of_path3 ?rev layer]

    Create a mesh from a single path (a closed loop of {!Vec2.t}), returning a
    {!type:t} with a single face including all of the points. Face winding order
    is reversed if [rev] is [true]. This can be useful for producing a flat
    patch mesh to be combined with other meshes to produce a complete shape. *)
val of_path2 : ?rev:bool -> Path2.t -> t

(** [of_path3 ?rev layer]

    Create a mesh from a single path (a closed loop of {!Vec3.t}, should be
   coplanar though it is not confirmed), returning a {!type:t} with a single
   face including all of the points. Face winding order is reversed if [rev] is
   [true]. This can be useful for producing a flat patch mesh to be combined
   with other meshes to produce a complete shape. *)
val of_path3 : ?rev:bool -> Path3.t -> t

(** [of_poly2 ?rev poly]

    Create a mesh from a 2d polygon. If [poly] does not have any holes, then
    this is equivalent to {!Mesh.of_path2}. If there are holes, polyhole
    partitioning is performed to determine a set of faces that can close the
    points.

    Adapted from:
    https://github.com/RonaldoCMP/Polygon-stuffs/blob/master/polyHolePartition.scad *)
val of_poly2 : ?rev:bool -> Poly2.t -> t

(** [of_poly3 ?rev poly]

    Create a mesh from a 3d polygon. If [poly] does not have any holes, then
    this is equivalent to {!Mesh.of_path3}. If there are holes, polyhole
    partitioning is performed to determine a set of faces that can close the
    points.

    Adapted from:
    https://github.com/RonaldoCMP/Polygon-stuffs/blob/master/polyHolePartition.scad *)
val of_poly3 : ?rev:bool -> Poly3.t -> t

(** [of_polygons polys]

    Create a polyhedron mesh from a list of polygonal point faces. *)
val of_polygons : Path3.t list -> t

(** {1 Sweeps and Extrusions with roundovers} *)

module Cap : sig
  (** Configuration module for declaring how extrusions from 2d to 3d via
    {!Mesh.sweep} should be capped off. *)

  (** Offset diameter [d] (positive or negative), and corresponding vertical
    step [z] (enforced positive only when consumed). *)
  type offset =
    { d : float
    ; z : float
    }

  (** A list of {!offset} describing a 3d end-cap extrusion roundover. *)
  type offsets

  (** Specifies how holes in the end-cap should be treated, either relative to
    the outer shape, or independantly. When multiple holes are present, [`Mix]
    allows each one to be specified separately, to treat all the same, use the
    other variants directly. *)
  type holes =
    [ `Same (** Offset [d] and [z] values from outer are copied. *)
    | `Flip
      (** Offset [d] and [z] values from outer are copied, but [d] sign is flipped. *)
    | `Custom of offsets
      (** Supplies a different set of offsets for the holes. Note that this
    should have the same [z] values, or else the roundover mesh will not be well
    formed. *)
    | `Mix of [ `Same | `Flip | `Custom of offsets ] list
    ]

  type poly =
    { outer : offsets
    ; holes : holes
    }

  (** Specifies whether an end of the extrusion should be left [`Empty], sealed
    with a [`Flat] face, or [`Round]ed over with a given offset specification. *)
  type poly_spec =
    [ `Empty
    | `Flat
    | `Round of poly
    ]

  type caps =
    { top : poly_spec
    ; bot : poly_spec
    }

  (** Top-level configuration type for {!Mesh.sweep}, allowing for the end-caps
    to be specified using the types above, or to simply loop the first and last
    layers of the mesh together (see: {!type:row_wrap} [`Loop] as used by
    {!of_layers}). *)
  type t =
    [ `Looped
    | `Caps of caps
    ]

  val chamf
    :  ?angle:float
    -> ?cut:float
    -> ?width:float
    -> ?height:float
    -> unit
    -> offsets

  val circ : ?fn:int -> [< `Cut of float | `Radius of float ] -> offsets
  val tear : ?fn:int -> [< `Cut of float | `Radius of float ] -> offsets
  val bez : ?curv:float -> ?fn:int -> [< `Cut of float | `Joint of float ] -> offsets
  val custom : offset list -> offsets
  val round : ?holes:holes -> offsets -> [> `Round of poly ]
  val looped : t
  val capped : top:poly_spec -> bot:poly_spec -> t
  val flat_caps : t
  val open_caps : t
end

val sweep
  :  ?check_valid:[ `Quality of int | `No ]
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?offset_mode:[< `Chamfer | `Delta | `Radius > `Radius ]
  -> ?spec:Cap.t
  -> transforms:MultMatrix.t list
  -> Poly2.t
  -> Mesh0.t

val linear_extrude
  :  ?check_valid:[ `Quality of int | `No ]
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?slices:int
  -> ?scale:Vec2.t
  -> ?twist:float
  -> ?center:bool
  -> ?offset_mode:[< `Chamfer | `Delta | `Radius > `Radius ]
  -> ?caps:Cap.caps
  -> height:float
  -> Poly2.t
  -> Mesh0.t

val helix_extrude
  :  ?check_valid:[ `Quality of int | `No ]
  -> ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?scale:Vec2.t
  -> ?twist:float
  -> ?offset_mode:[< `Chamfer | `Delta | `Radius > `Radius ]
  -> ?caps:Cap.caps
  -> ?left:bool
  -> n_turns:int
  -> pitch:float
  -> ?r2:float
  -> float
  -> Poly2.t
  -> Mesh0.t

val path_extrude
  :  ?check_valid:[ `Quality of int | `No ]
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?offset_mode:[< `Chamfer | `Delta | `Radius > `Radius ]
  -> ?spec:Cap.t
  -> ?euler:bool
  -> ?scale:Vec2.t
  -> ?twist:float
  -> path:Path3.t
  -> Poly2.t
  -> Mesh0.t

val prism
  :  ?debug:bool
  -> ?fn:int
  -> ?k:float
  -> ?k_bot:float
  -> ?k_top:float
  -> ?k_sides:[< `Flat of float | `Mix of float list > `Flat ]
  -> ?joint_bot:float * float
  -> ?joint_top:float * float
  -> ?joint_sides:[< `Flat of float * float | `Mix of (float * float) list > `Flat ]
  -> Vec3.t list
  -> Vec3.t list
  -> Mesh0.t

(** {1 Function Plotting} *)

val cartesian_plot
  :  min_x:float
  -> x_steps:int
  -> max_x:float
  -> min_y:float
  -> y_steps:int
  -> max_y:float
  -> (x:float -> y:float -> float)
  -> t

val polar_plot : ?r_step:float -> max_r:float -> (r:float -> a:float -> float) -> t

val axial_plot
  :  ?fn:int
  -> min_z:float
  -> z_steps:int
  -> max_z:float
  -> (z:float -> a:float -> float)
  -> t

(** {1 Mesh Utilities} *)

val join : t list -> t
val merge_points : ?eps:float -> t -> t
val add_face : int list -> t -> t
val add_faces : int list list -> t -> t
val rev_faces : t -> t
val volume : t -> float
val area : t -> float
val centroid : ?eps:float -> t -> Vec3.t

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
val show_points : (int -> 'a Scad.t) -> t -> 'a Scad.t
val to_scad : ?convexity:int -> t -> Scad.d3
