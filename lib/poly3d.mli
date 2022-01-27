type layer = Vec3.t list

type t =
  { n_layers : int
  ; n_facets : int
  ; points : Vec3.t list
  ; faces : int list list
  }

(** [of_layers ?closed layers]

    Create a polyhedron from a list of layers (counter_clockwise loops of 3d
    points). Setting [closed] to true will connect the open faces of the first
    and last layers (defaults to [false]). *)
val of_layers : ?closed:bool -> layer list -> t

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
