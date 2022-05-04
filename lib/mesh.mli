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

    The earcutting algorithm used to partition the polygon into faces is a port
    of RonaldoCMP's work found
    {{:https://github.com/RonaldoCMP/Polygon-stuffs/blob/master/polyHolePartition.scad}
    here}. *)
val of_poly2 : ?rev:bool -> Poly2.t -> t

(** [of_poly3 ?rev poly]

    Create a mesh from a 3d polygon. If [poly] does not have any holes, then
    this is equivalent to {!Mesh.of_path3}. If there are holes, polyhole
    partitioning is performed to determine a set of faces that can close the
    points.

    The earcutting algorithm used to partition the polygon into faces is a port
    of RonaldoCMP's work found
    {{:https://github.com/RonaldoCMP/Polygon-stuffs/blob/master/polyHolePartition.scad}
    here}. *)
val of_poly3 : ?rev:bool -> Poly3.t -> t

(** [of_polygons polys]

    Create a polyhedron mesh from a list of polygonal point faces. *)
val of_polygons : Path3.t list -> t

(** {1 Sweeps and Extrusions with roundovers}

Extrusions from 2d to 3d with optional roundovers based on the implementations found in
the BOSL2 library's [offset_sweep] functions found in
{{:https://github.com/revarbat/BOSL2/blob/master/rounding.scad} rounding.scad}. *)

module Cap : sig
  (** Configuration module for declaring how extrusions from 2d to 3d via
    {!Mesh.sweep} should be capped off. *)

  (** Offset diameter [d] (positive or negative), and corresponding vertical
    step [z] (enforced positive only when consumed). *)
  type offset =
    { d : float
    ; z : float
    }

  type offset_mode =
    | Delta
    | Chamfer
    | Radius of
        { fn : int option
        ; fs : float option
        ; fa : float option
        }

  (** A list of {!offset} describing a 3d end-cap extrusion roundover.
    Abstracted to enforce use of constructors. *)
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
    ; mode : offset_mode
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

  (** [chamf ?angle ?cut ?width ?height ()]

       Create offsets that will produce a chamfer roundover. One of [cut]
   (amount of corner "cut off"), [width] (horizontal distance of chamfer), or
   [height] (vertical distance of chamfer) can be specified to be used in
   conjunction with [angle] (default = pi / 4), or [width] and [height] can be
   provided together, in which case [angle] is not used. *)
  val chamf
    :  ?angle:float
    -> ?cut:float
    -> ?width:float
    -> ?height:float
    -> unit
    -> offsets

  (** [circ ?fn roundover]

      Create offsets that will produce a circular roundover, according to
    either a given [`Cut] distance, or a [`Radius], over [fn] steps (default 16). *)
  val circ : ?fn:int -> [< `Cut of float | `Radius of float ] -> offsets

  (** [tear ?fn roundover]

      Create offsets that will produce a teardrop rounover (circular, endind in
    a chamfer). *)
  val tear : ?fn:int -> [< `Cut of float | `Radius of float ] -> offsets

  (** [bez ?curv ?fn spec]

      Create offsets that will produce a smooth bezier roundover. The amplitude
    of the curve can be given by [`Cut] (amount of corner "cut off"), or
    [`Joint] (distance vertically the bezier covers). [curv] is the curvature
    smoothness parameter, ranging from gradual [0.], to abrupt [1.] (default [0.5]). *)
  val bez : ?curv:float -> ?fn:int -> [< `Cut of float | `Joint of float ] -> offsets

  (** [custom offsets]

      Sanitize (enforce positive and increasing [z] values) and pack a list of
      {!type:offset}. *)
  val custom : offset list -> offsets

  (** [unwrap_offsets offsets]

      Retrieve list of {!type:offset} from the abstract {!type:offsets}. *)
  val unwrap_offsets : offsets -> offset list

  (** [round ?mode ?holes offsets]

      Construct a roundover {!type:poly_spec}. *)
  val round : ?mode:offset_mode -> ?holes:holes -> offsets -> [> `Round of poly ]

  (** [looped]

      [`Looped], indicating that the top should loop around to the bottom of
      a sweep. *)
  val looped : t

  (** [capped ~top ~bot]

      Construct a {!Cap.t} specifying how the [top] and [bot] caps of a sweep
      extrusion should be sealed off (or not). *)
  val capped : top:poly_spec -> bot:poly_spec -> t

  (** [flat_caps]

      Default {!Cap.t} configuration for flat (no roundover) sealed caps. *)
  val flat_caps : t

  (** [open_caps]

      Default {!Cap.t} configuration for unsealed caps (open ends). *)
  val open_caps : t
end

(** [sweep ?check_valid ?winding ?fn ?fs ?fa ?spec ~transforms poly]

    Sweep a 2d polygon into a 3d mesh by applying a sequence of [transforms] to
   the original shape. The [winding] parameter can be used to set automatic
   enforcement of polygon winding direction, which will impact the winding of
   the generated faces of the mesh. What is done with the endcaps can be
   specified with [spec]. By default the ends of the extrusion are sealed with
   flat faces, but they can instead be looped to eachother, left empty, or
   rounded over.

   Relevant when roundovers are on:
    - [check_valid] determines whether validity checks are performed during
    offset operations, see {!Path2.offset}, for cap roundovers (if specified).
    - [fn], [fs], and [fa] determine number of points used when generating new
    points in the {!Path2.offset} roundover, if [`Radius] mode is being used. *)
val sweep
  :  ?check_valid:[ `Quality of int | `No ]
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?spec:Cap.t
  -> transforms:MultMatrix.t list
  -> Poly2.t
  -> Mesh0.t

(** [linear_extrude ?check_valid ?winding ?fa ?slices ?scale ?twist
    ?center ?caps ~height poly]

    Vertically extrude a 2d polygon into a 3d mesh. [slices], [scale], [twist],
    [center], and [height] parameters are analogous to those found on
    {!Scad.linear_extrude}. See {!sweep} for explaination of shared parameters
    (note: [caps] is a subset of [spec], since the ends of a linear extrusion
    cannot be looped) *)
val linear_extrude
  :  ?check_valid:[ `Quality of int | `No ]
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?fa:float
  -> ?slices:int
  -> ?scale:Vec2.t
  -> ?twist:float
  -> ?center:bool
  -> ?caps:Cap.caps
  -> height:float
  -> Poly2.t
  -> Mesh0.t

(** [helix_extrude ?check_valid ?fn ?fs ?fa ?scale ?twist
     ?caps ?left ~n_turns ~pitch ?r2 r1 poly]

    Helical extrusion of a 2d polygon into a 3d mesh. Like
   {!Mesh.linear_extrude}, but following a path generated with
   {!Path3.helix}. *)
val helix_extrude
  :  ?check_valid:[ `Quality of int | `No ]
  -> ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?scale:Vec2.t
  -> ?twist:float
  -> ?caps:Cap.caps
  -> ?left:bool
  -> n_turns:int
  -> pitch:float
  -> ?r2:float
  -> float
  -> Poly2.t
  -> Mesh0.t

(** [path_extrude ?check_valid ?winding ?spec ?euler ?scale ?twist ~path poly]

    Extrude a 2d polygon along the given [path] into a 3d mesh. This is a
    convenience function that composes transform generation using
    {!Path3.to_transforms} with {!Mesh.sweep}. *)
val path_extrude
  :  ?check_valid:[ `Quality of int | `No ]
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?spec:Cap.t
  -> ?euler:bool
  -> ?scale:Vec2.t
  -> ?twist:float
  -> path:Path3.t
  -> Poly2.t
  -> Mesh0.t

(** [prism ?debug ?fn ?k ?k_bot ?k_top ?k_sides ?joint_bot ?joint_top
  ?joint_sides bottom top]

    Create a prism with continuous curvature rounding from the given [bottom]
    and [top] paths. The edges running between the bounding polygons must
    produce a valid polyhedron with coplanar side faces. [joint_] parameters
    determine the distance away from the corner that curvature begins, and [k]
    parameters set the smoothness of the curvature.

    - [debug] can be set to [true] to skip validity checks that would otherwise
    raise exceptions on failure, so a mesh can still be obtained for
    inspection. *)
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

(** {1 Function Plotting}

    Ported from the {{:https://github.com/rcolyer/plot-function} PlotFunction}
    library by Ryan Colyer. *)

(** [cartesian_plot ~min_x ~x_steps ~max_x ~min_y ~y_steps ~max_y f]

    Create a mesh of the function [f] (from x and y to z) over the ranges of x
   and y defined by the rest of the parameters. *)
val cartesian_plot
  :  min_x:float
  -> x_steps:int
  -> max_x:float
  -> min_y:float
  -> y_steps:int
  -> max_y:float
  -> (x:float -> y:float -> float)
  -> t

(** [polar_plot ?r_step ~max_r f]

    Create a mesh of the function [f] (from radius and angle to z) between the
   z-axis and the radius [max_r], with the minimum radial step [r_step]. *)
val polar_plot : ?r_step:float -> max_r:float -> (r:float -> a:float -> float) -> t

(** [axial_plot ?fn ~min_z ~z_step ~max_z f]

    Create a mesh of the function [f] (from z-height and angle to radius). [fn]
    sets the number of angular steps around the z-axis. *)
val axial_plot
  :  ?fn:int
  -> min_z:float
  -> z_steps:int
  -> max_z:float
  -> (z:float -> a:float -> float)
  -> t

(** {1 Mesh Utilities} *)

(** [join ts]

    Join a list of meshes. This is not a boolean operation, it is simply
    collecting the points from each and updating face indices accordingly.
    Intended for use when building a closed mesh from a set of partial meshes. *)
val join : t list -> t

(** [merge_points ?eps t]

    Eliminate duplicate points (less than [eps] distance apart) from [t]. *)
val merge_points : ?eps:float -> t -> t

(** [add_face face t]

    Add a single face to the mesh [t]. *)
val add_face : int list -> t -> t

(** [add_faces faces t]

    Add a list of faces to the mesh [t]. *)
val add_faces : int list list -> t -> t

(** [rev_faces t]

    Flip all faces the mesh. *)
val rev_faces : t -> t

(** [volume t]

    Calculate the volume of the mesh [t]. *)
val volume : t -> float

(** [area t]

    Calculate the surface area of the mesh [t]. *)
val area : t -> float

(** [centroid ?eps t]

    Calculate the centroid of the mesh [t]. *)
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

(** {1 Debugging helpers} *)

val show_points : (int -> 'a Scad.t) -> t -> 'a Scad.t

(** {1 Output} *)

val to_scad : ?convexity:int -> t -> Scad.d3
