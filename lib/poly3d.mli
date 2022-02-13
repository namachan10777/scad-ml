type t

val empty : t
val n_points : t -> int
val points : t -> Vec3.t list
val faces : t -> int list list
val make : points:Vec3.t list -> faces:int list list -> t

(** [of_layers ?closed layers]

    Create a {!type:t} representing a polyhedron from a list of layers
    (counter_clockwise loops of 3d points). Setting [closed] to true will connect
    the open faces of the first and last layers (defaults to [false]). The
    generated faces wrap around the ends of the layers, creating an enclosed
    shape which is generally ready to be passed along to {!Poly3d.to_scad}
    without further modification. If [layers] is empty, a {!empty} is returned.
    Throws [Invalid_argument] if [layers] contains only one layer, or if it is
    not rectangular (any layer differs in length). *)
val of_layers : ?closed:bool -> Vec3.t list list -> t

(** [tri_mesh ?closed rows]

    Create a triangular mesh from a list of rows, where each row can differ in
    length relative to its neighbours by up to 2. Since the rows can be ragged,
    no (columnar) wrapping is done, thus they are best described as rows, rather
    than layers as with {!of_layers} which produces an enclosed polyhedron.
    Instead, this function is useful for the generation of triangular patches
    that can be joined with one another to create a complete polyhedron. Setting
    [closed] to true will generate faces between the last and first rows, so long
    as their lengths differ by no more than 2. Throws [Invalid_argument] if a row
    length delta of greater than 2 is encountered. *)
val tri_mesh : ?closed:bool -> Vec3.t list list -> t

(** [mesh_of_layer ?reverse layer]

    Create a mesh from a single layer (a closed loop of {!Vec3.t}), returning a
    {!type:t} with a single face including all of the points. Face winding order
    is reversed if [reverse] is [true]. This can be useful for producing a flat
    patch mesh to be combined with other meshes to produce a complete shape. *)
val mesh_of_layer : ?reverse:bool -> Vec3.t list -> t

(** [sweep ?closed ?convexity ~transforms shape]

    Create a polyhedron by sweeping the given [shape], described as a 2d
    polygon of {!Vec2.t}s on the XY plane, by applying the [transforms] in turn.
    Setting [closed] to true will connect the open faces of the beginning and
    end of the sweep (defaults to [false]). *)
val sweep
  :  ?winding:[ `CCW | `CW | `NoCheck ]
  -> ?closed:bool
  -> transforms:MultMatrix.t list
  -> Vec2.t list
  -> t

val linear_extrude
  :  ?slices:int
  -> ?fa:float
  -> ?scale:Vec2.t
  -> ?twist:float
  -> ?center:bool
  -> height:float
  -> (float * float) list
  -> t

val helix_extrude
  :  ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?scale:float * float
  -> ?twist:float
  -> ?left:bool
  -> n_turns:int
  -> pitch:float
  -> ?r2:float
  -> float
  -> (float * float) list
  -> t

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

val join : t list -> t
val add_face : int list -> t -> t
val add_faces : int list list -> t -> t
val rev_faces : t -> t
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
