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
val sweep : ?closed:bool -> transforms:MultMatrix.t list -> Vec2.t list -> t

val linear_extrude
  :  ?slices:int
  -> ?scale:Vec2.t
  -> ?twist:float
  -> ?center:bool
  -> height:float
  -> (float * float) list
  -> t

val to_scad : ?convexity:int -> t -> Scad.d3
