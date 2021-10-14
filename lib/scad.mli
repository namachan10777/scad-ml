(** The core module of this library. Containing the core type, and all operations upon it.

    OpenSCAD objects and the operations upon them are represented by the
    recursive type {!type:t}, which can then finally be translated into a scad
    script. As this transpiles down to OpenSCAD, the
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language}
    User Manual} can be referred to for additional information about most of the
    functions (and examples of output) made available here. *)

type two_d = TwoD
type three_d = ThreeD

(** The scad type is kept abstract, as the available constructor functions
    provide much less cumbersome means of building up models.*)
type scad

(** This GADT allows scads to be tagged as 2D or 3D, restricting usage of functions
    that should only apply to one or the other, and preventing mixing during
    boolean operations. *)
type 'space t

(** Two-dimensional scad *)
type d2 = two_d t

(** Three-dimensional scad *)
type d3 = three_d t

(** {1 A note on special facet parameters}

    The optional parameters ?fa, ?fs, and ?fn correspond to the OpenSCAD
    "special parameters" governing the number of facets used to generate arcs
    ($fa, $fs, and $fn respectively). Where present, they govern the following:

    - ?fa is the minimum angle for a fragment. Note that this should be given in
      radians here (as opposed to degrees in the OpenSCAD language).
    - ?fs is the minimum size/length of a fragment. Even if ?fa is given, and
      very small, this parameter will limit how small the generated fragments
      can be.
    - ?fn will set the absolute number of fragments to be used (causing the
      previous two parameters to be ignored. *)

(** {1 3d shape primitives} *)

(** [cube ?center dimensions]

    Creates a cube in the first octant, with the given xyz [dimensions]. When
    [center] is true, the cube is centered on the origin. *)
val cube : ?center:bool -> float * float * float -> d3

(** [sphere ?fa ?fs ?fn radius]

    Creates a sphere with given [radius] at the origin of the coordinate system. *)
val sphere : ?fa:float -> ?fs:float -> ?fn:int -> float -> d3

(** [cylinder ?center ?fa ?fs ?fn radius height]

     Creates a cylinder centered about the z axis. When center is true, it will
     also be centered vertically, otherwise the base will sit upon the XY
     plane. *)
val cylinder : ?center:bool -> ?fa:float -> ?fs:float -> ?fn:int -> float -> float -> d3

(** [cone ?center ?fa ?fs ?fn ~height r1 r2 ]

    Creates a cone (using the cylinder primitive) with bottom radius [r1] and
    top radius [r2]. *)
val cone
  :  ?center:bool
  -> ?fa:float
  -> ?fs:float
  -> ?fn:int
  -> height:float
  -> float
  -> float
  -> d3

(** [polyhedron points faces]

    A polyhedron is the most general 3D primitive solid. It can be used to
    create any regular or irregular shape including those with concave as well
    as convex features. Curved surfaces are approximated by a series of flat

    The {!Vec3.t} list of coordinate [points] represents the vertices of the
    shape. How these points define the surface of the generated shape is
    specified by the [faces] (0-based) index lists. Each element of [faces]
    should define a face in the same order (from the perspective of viewing the
    drawn shape from the outside, OpenSCAD prefers clockwise ordering) face
    using the points at the indices in [points]. These may be defined in any
    order, as long as enough are provided to fully enclose the shape.

    The optional [?convexity] parameter specifies the maximum number of faces a
    ray intersecting the object might penetrate. This parameter is needed only
    for correct display of the object in OpenCSG preview mode. It has no effect
    on the polyhedron rendering. For display problems, setting it to 10 should
    work fine for most cases.

    If you are having trouble, please see the
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language#Debugging_polyhedra}
    debugging polyhedra} section of the OpenSCAD user manual. *)
val polyhedron : ?convexity:int -> Vec3.t list -> int list list -> d3

(** {1 2d shape primitives} *)

(** [square ?center dimensions]

    Creates a square or rectangle in the first quadrant, with given xyz
    [dimensions]. When [?center] is true the square is centered on the
    origin. *)
val square : ?center:bool -> float * float -> d2

(** [circle ?fa ?fs ?fn radius]

    Creates a circle with given [radius] at the origin. *)
val circle : ?fa:float -> ?fs:float -> ?fn:int -> float -> d2

(** [polygon ?convexity ?paths points]

    Creates a multiple sided shape from a list of xy [points]. If [?paths] is
    not specified, then all points will be used in the order that they appear. A
    single path list specifies the order to traverse the [points] to draw the
    outline of the polygon. Any additional lists of indices will describe
    secondary shapes to be subtracted from the first. In this way, holes can be
    placed in the shape without a subsequent boolean {!difference} operation.

    For information on the [?convexity], please see the documentation for
    {!polyhedron}. *)
val polygon : ?convexity:int -> ?paths:int list list -> (float * float) list -> d2

(** [text ?size ?font ?halign ?valign ?spacing ?direction ?language ?script ?fn str]

    Creates 2D geometric text object with contents [str], using a named font
    installed on the local system or provided as a separate font file.
    - [?size] specifies the ascent (height above baseline). Default is 10.
    - [?font] is the name of the font that should be used. This is not the name
      of the font file, but the logical font name (internally handled by the
      fontconfig library).
    - [?halign] and [?valign] set the horizontal and vertical alignments of the text.
      Defaults are [Left] and [Baseline] respectively.
    - [?spacing] gives a factor by which to increase/decrease the character spacing.
      A value of 1.0 would result in normal spacing.
    - [?direction] sets the text flow. Default is [LeftToRight].
    - [?language] sets the language of the text. Default is "en".
    - [?script] sets the script of the text. Default is "latin". *)
val text
  :  ?size:float
  -> ?font:string
  -> ?halign:Text.h_align
  -> ?valign:Text.v_align
  -> ?spacing:float
  -> ?direction:Text.direction
  -> ?language:string
  -> ?script:string
  -> ?fn:int
  -> string
  -> d2

(** {1 Transformations}

    These functions can be applied freely to both 2d and 3d shapes. *)

(** [translate p t]

    Moves [t] along the vector [p]. *)
val translate : Vec3.t -> 's t -> 's t

(** [rotate r t]

    Performs an Euler rotation (x -> y -> z) on [t] with the angles [r]
    (in radians). *)
val rotate : Vec3.t -> 's t -> 's t

(** [rotate_about_pt r p t]

    Translates [t] along the vector [p], rotating the resulting shape by [r],
    and finally, moving back along the vector [p]. Functionally, rotating about
    the point in space arrived at by the initial translation along the vector
    [p]. *)
val rotate_about_pt : Vec3.t -> Vec3.t -> 's t -> 's t

(** [vector_rotate ax r t]

    Rotates [t] about the arbitrary axis [ax] by the angle [r]. *)
val vector_rotate : Vec3.t -> float -> 's t -> 's t

(** [vector_rotate_about_pt ax r p t]

    Translates [t] along the vector [p], rotating the resulting shape around
    [ax] by angle [r], and finally, moving back along the vector [p].
    Functionally, rotating about the point in space arrived at by the initial
    translation along the vector [p]. *)
val vector_rotate_about_pt : Vec3.t -> float -> Vec3.t -> 's t -> 's t

(** [multmatrix mat t]

    Multiplies the geometry [t] with the given 4x3 affine transformation matrix.
    You can find a detailed explanation of how these are formed and interpreted
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language#multmatrix}
    here}. *)
val multmatrix : MultMatrix.t -> 's t -> 's t

(** [mirror ax t]

    Mirrors [t] on a plane through the origin, defined by the normal vector
    [ax]. *)
val mirror : float * float * float -> 's t -> 's t

(** [quaternion q t]

    Applys the quaternion rotation [q] to [t]. *)
val quaternion : Quaternion.t -> 's t -> 's t

(** [quaternion_about_pt q p t]

    Translates [t] along the vector [p], rotating the resulting shape with the
    quaternion [q], and finally, moving back along the vector [p]. Functionally,
    rotating about the point in space arrived at by the initial translation
    along the vector [p]. *)
val quaternion_about_pt : Quaternion.t -> Vec3.t -> 's t -> 's t

(** [scale factors t]

    Scales [t] by the given [factors] in xyz. *)
val scale : float * float * float -> 's t -> 's t

(** [resize dimensions t]

    Adjusts the size of [t] to match the given [dimensions]. *)
val resize : float * float * float -> 's t -> 's t

(** [offset ?chamfer offset t]

    Generates a new 2D interior or exterior outline from an existing outline [t].
    - [`Delta d] will create a new outline whose sides are a fixed distance [d]
      (+ve out, -ve in) from the original outline.
    - [`Radius r] creates a new outline as if a circle of some radius [r] is
      rotated around the exterior ([r > 0]) or interior ([r < 0]) original
      outline.
    - [?chamfer] determines whether edges should be chamfered off when using
      [`Delta] offsets.

    Helpful diagrams of what each of these offset styles and chamfering look
    like can be found
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language#offset}
    here}. *)
val offset : ?chamfer:bool -> [ `Delta of float | `Radius of float ] -> d2 -> d2

(** [color ?alpha color t]

    Displays [t] with the specified [color] and [?alpha] value. This is only
    used for the F5 preview as CGAL and STL (F6, render) do not currently
    support color. Defaults to opaque (alpha = 1.0). *)
val color : ?alpha:float -> Color.t -> 's t -> 's t

(** {1 Boolean Combination}

    Perform boolean operations between shapes of the same dimension (non-mixing
    of 2d and 3d shapes is enforced by the the GADT {!type:t}. {!Note that the
    polymorphic versions of {!union}, {!minkowski}, {!hull}, and
    {!intersection}, throw exceptions when the input list is empty. If empty
    list inputs are expected, then use the appropriate [_2d] or [_3d]
    variant. *)

(** [union ts]

    Creates the union/sum (logical {b or }) [ts]. Throws an exception if [ts] is
    empty, use {!union_2d} or {!union_3d} if you would like empty unions to pass
    silently.

    {b Note: } It is mandatory for all unions, explicit or implicit, that
    external faces to be merged not be coincident. Failure to follow this rule
    results in a design with undefined behavior, and can result in a render
    which is not manifold (with zero volume portions, or portions inside out),
    which typically leads to a warning and sometimes removal of a portion of the
    design from the rendered output. (This can also result in flickering effects
    during the preview.) This requirement is not a bug, but an intrinsic
    property of floating point comparisons and the fundamental inability to
    exactly represent irrational numbers such as those resulting from most
    rotations.

    The solution to this is to use a small value called an epsilon when
    merging adjacent faces to guarantee that they overlap, like so:
   {[
     let scad =
       let eps = 0.01 in
       union [ cube (1., 1., 1.)
             ; translate (1. -. eps, 0., 0.) (cube (2. +. eps, 2., 2.))
             ]
   ]} *)
val union : 's t list -> 's t

val union_2d : d2 list -> d2
val union_3d : d3 list -> d3

(** [minkowski ts]

    Displays the minkowski sum of [ts]. Throws an exception if [ts] is empty,
    use {!minkowski_2d} or {!minkowski_3d} if you would like empty minkowski
    sums to pass silently. *)
val minkowski : 's t list -> 's t

val minkowski_2d : d2 list -> d2
val minkowski_3d : d3 list -> d3

(** [hull ts]

    Displays the convex hull of [ts]. Throws an exception if [ts] is empty, use
    {!hull_2d} or {!hull_3d} if you would like empty hulls to pass silently. *)
val hull : 's t list -> 's t

val hull_2d : d2 list -> d2
val hull_3d : d3 list -> d3

(** [difference t sub]

    Subracts the shapes of [sub] from [t] (logical {b and not }).

    {b Note: } It is mandatory that surfaces that are to be removed by a
    difference operation have an overlap, and that the negative piece being
    removed extends fully outside of the volume it is removing that surface
    from. Failure to follow this rule can cause preview artifacts and can result
    in non-manifold render warnings or the removal of pieces from the render
    output. See the description above in union for why this is required and an
    example of how to do this by this using a small epsilon value. *)
val difference : 's t -> 's t list -> 's t

(** [intersection ts]

    Creates an in intersection of [ts]. This keeps the overlapping portion
    (logical {b and }). Only the area which is common or shared by {b all } shapes
    are retained. Throws an exception if [ts] is empty, use {!intersection_2d}
    or {!intersection_3d} if you would like empty intersections to pass
    silently. *)
val intersection : 's t list -> 's t

val intersection_2d : d2 list -> d2
val intersection_3d : d3 list -> d3

(** {1 3d to 2d} *)

(** [projection ?cut t]

    Project a 3D model [t] to the XY plane, resulting in an infinitely thin 2D
    shape, which can then be extruded back into 3D, or rendered and exported as
    a [.dxf]. If [?cut] is true, only points with z=0 (a slice of [t] where it
    intersects with the XY plane) are considered. When [?cut] is false (the
    default when not specified), then points above and below the XY plane will
    be considered in forming the projection. *)
val projection : ?cut:bool -> d3 -> d2

(** {1 2d to 3d extrusions} *)

(** [linear_extrude ?height ?center ?convexity ?twist ?slices ?scale ?fn t]

    Takes a 2D object [t], and extrudes it upwards from the XY plane to
    [?height]. If [?center] is true, the resulting 3D object is centered around
    the XY plane, rather than resting on top of it.
    - [?twist] rotates the shape by the specified angle as it is extruded
      upwards
    - [?slices] specifies the number of intermediate points along the Z axis of
      the extrusion. By default this increases with the value of [?twist],
      though manual refinement my improve results.
    - [?scale] expands or contracts the shape in X and Y as it is extruded
      upward. Default is (1., 1.), no scaling.
    - [?convexity]: see {!polyhedron} documentation. *)
val linear_extrude
  :  ?height:float
  -> ?center:bool
  -> ?convexity:int
  -> ?twist:int
  -> ?slices:int
  -> ?scale:float * float
  -> ?fn:int
  -> d2
  -> d3

(** [rotate_extrude ?angle ?convexity ?fa ?fs ?fn t]

    Spins a 2D shape [t] around the Z-axis in an arc of [?angle] (default = 2π)
    to form a solid which has rotational symmetry. Since [t] is actually 2D (and
    does not really exist in Z), it is more like it is spun around the Y-axis to
    form the solid, which is then placed so that its axis of rotation lies along
    the Z-axis. For this reason, [t] {b must } lie completely on either the
    right (recommended) or the left side of the Y-axis. Further explaination and
    examples can be found
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language#Rotate_Extrude}
    here}. *)
val rotate_extrude
  :  ?angle:float
  -> ?convexity:int
  -> ?fa:float
  -> ?fs:float
  -> ?fn:int
  -> d2
  -> d3

(** [import_2d ?dxf_layer ?convexity file]

    Imports a [file] for use in the current OpenSCAD model. The file extension
    is used to determine which type. If [file] is a [.dxf], [?dxf_layer] can be
    used to indicate a specific layer for import. Throws exception if the
    extension does not match (case insensitive) one of the following 3D formats:
    - DXF
    - SVG {b Note: } {i Requires version 2019.05 of OpenSCAD } *)
val import_2d : ?dxf_layer:string -> ?convexity:int -> string -> d2

(** [import_3d  ?convexity file]

    Imports [file] for use in the current OpenSCAD model. The file extension is
    used to determine which type. Throws exception if the extension does not
    match (case insensitive) one of the following 3D formats:
    - STL (both ASCII and Binary)
    - OFF
    - AMF {b Note: } {i Requires version 2019.05 of OpenSCAD }
    - 3MF {b Note: } {i Requires version 2019.05 of OpenSCAD } *)
val import_3d : ?convexity:int -> string -> d3

(** [to_string t]

    Convert the scad [t] to a string in the OpenSCAD language. *)
val to_string : 's t -> string

(** [write oc t]

    Write the scad [t] to the given [out_channel] (typically a file), as an
    OpenSCAD script (using {!to_string}). *)
val write : out_channel -> 's t -> unit

module Infix : sig
  (** [t |>> p]

    Infix {!translate} *)
  val ( |>> ) : 's t -> Vec3.t -> 's t

  (** [t |@> r]

    Infix {!rotate} *)
  val ( |@> ) : 's t -> Vec3.t -> 's t
end
