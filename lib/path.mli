val total_travel : Vec3.t list -> float
val cummulative_travel : Vec3.t list -> float list
val to_continuous : Vec3.t list -> float -> Vec3.t

val of_continuous
  :  ?init:Vec3.t list
  -> ?rev:bool
  -> ?fn:int
  -> (float -> Vec3.t)
  -> Vec3.t list

val resample : freq:[< `N of int | `Spacing of float ] -> Vec3.t list -> Vec3.t list

(** [to_transforms t]

    Generate list of transformations that can be applied to three-dimensional
    vectors ({!Vec3.t} via {!MultMatrix.transform}) or shapes ({!Scad.d3} via
    {!Scad.multmatrix}), to move them along the [path] (intended to be applied to
    the vector/shape from its original position each time). Tangents are used to
    calculate appropriate rotations for each translation. *)
val to_transforms : ?scale:Vec2.t -> ?twist:float -> Vec3.t list -> MultMatrix.t list
