(** [to_transforms t]

    Generate list of transformations that can be applied to three-dimensional
    vectors ({!Vec3.t} via {!MultMatrix.transform}) or shapes ({!Scad.d3} via
    {!Scad.multmatrix}), to move them along the [path] (intended to be applied to
    the vector/shape from its original position each time). Tangents are used to
    calculate appropriate rotations for each translation. *)
val to_transforms : Vec3.t list -> MultMatrix.t list
