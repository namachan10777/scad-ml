type t

type row_wrap =
  [ `Loop
  | `Both
  | `None
  | `Top
  | `Bot
  ]

val empty : t
val n_points : t -> int
val points : t -> Vec3.t list
val faces : t -> int list list
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
    than layers as with {!of_layers} which produces an enclosed polyhedron.
    Instead, this function is useful for the generation of triangular patches
    that can be joined with one another to create a complete polyhedron. Setting
    [looped] to true will generate faces between the last and first rows, so long
    as their lengths differ by no more than 2. Face winding order is reversed if
    [reverse] is [true]. Throws [Invalid_argument] if a row length delta of
    greater than 2 is encountered. *)
val of_ragged : ?looped:bool -> ?rev:bool -> Vec3.t list list -> t

(** [of_path2 ?rev layer]

    *)
val of_path2 : ?rev:bool -> Path2.t -> t

(** [of_path3 ?rev layer]

    Create a mesh from a single layer (a closed loop of {!Vec3.t}), returning a
    {!type:t} with a single face including all of the points. Face winding order
    is reversed if [rev] is [true]. This can be useful for producing a flat
    patch mesh to be combined with other meshes to produce a complete shape. *)
val of_path3 : ?rev:bool -> Path3.t -> t

(** [of_poly2 ?rev poly]

    *)
val of_poly2 : ?rev:bool -> Poly2.t -> t

(** [of_poly3 ?rev poly]

    *)
val of_poly3 : ?rev:bool -> Poly3.t -> t

(** [of_polygons polys]

    Create a polyhedron mesh from a list of polygonal faces. *)
val of_polygons : Path3.t list -> t

(** [sweep ?winding ?loop ?convexity ~transforms shape]

    Create a polyhedron by sweeping the given [shape], described as a 2d
    polygon of {!Vec2.t}s on the XY plane, by applying the [transforms] in turn. *)
val sweep
  :  ?winding:[ `CCW | `CW | `NoCheck ]
  -> ?loop:bool
  -> transforms:MultMatrix.t list
  -> Vec2.t list
  -> t

val linear_extrude
  :  ?winding:[ `CCW | `CW | `NoCheck ]
  -> ?slices:int
  -> ?fa:float
  -> ?scale:Vec2.t
  -> ?twist:float
  -> ?center:bool
  -> height:float
  -> Vec2.t list
  -> t

val helix_extrude
  :  ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?scale:Vec2.t
  -> ?twist:float
  -> ?left:bool
  -> n_turns:int
  -> pitch:float
  -> ?r2:float
  -> float
  -> Vec2.t list
  -> t

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
val to_scad : ?convexity:int -> t -> Scad.d3
