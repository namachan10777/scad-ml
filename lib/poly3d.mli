(** [extrude_with_radius ?fn ~height ~r1 ~r2 scad]

    Perform a linear extrusion of [scad], with the addition of rounded end caps
    of radius [r1] (bottom) and [r2] (top), formed by [fn] steps of extrusions
    on the radially offset [scad]. Negative radius values will flare out,
    rather than rounding to a smaller face. Note that this function results in
    a slower shape than polyround extrusions, and high [fn] values will be
    required to achieve relatively smooth transitions. It can be useful though
    if you have a 2d scad already. *)
val extrude_with_radius
  :  ?fn:int
  -> height:float
  -> r1:float
  -> r2:float
  -> Scad.d2
  -> Scad.d3

(** [stitch_polyhedron ?closed ?convexity layers]

    Create a polyhedron from a list of layers (counter_clockwise loops of 3d
    points). Setting [closed] to true will connect the open faces of the first
    and last layers (defaults to [false]). [convexity] is passed along to
    {!Scad.polyhedron} if provided. *)
val stitch_polyhedron : ?closed:bool -> ?convexity:int -> Vec3.t list list -> Scad.d3

(** [sweep ?closed ?convexity ~transforms shape]

    Create a polyhedron by sweeping the given [shape], described as a 2d
    polygon of {!Vec2.t}s on the XY plane, by applying the [transforms] in turn.
    Setting [closed] to true will connect the open faces of the beginning and
    end of the sweep (defaults to [false]). [convexity] is passed along to
    {!Scad.polyhedron} if provided. *)
val sweep
  :  ?closed:bool
  -> ?convexity:int
  -> transforms:MultMatrix.t list
  -> Vec2.t list
  -> Scad.d3
