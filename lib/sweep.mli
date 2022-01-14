(** [transforms_of_path path]

    Generate list of transformations that can be applied to three-dimensional
   vectors ({!Vec3.t} via {!MultMatrix.transform}) or shapes ({!Scad.d3} via
   {!Scad.multmatrix}), to move them along the [path] (intended to be applied to
   the vector/shape from its original position each time). Tangents are used to
   calculate appropriate rotations for each translation. *)
val transforms_of_path : Vec3.t list -> MultMatrix.t list

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
