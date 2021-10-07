(** The core module of this library. Containing the core type, and all operations upon it.

    OpenSCAD objects and the operations upon them are represented by the
    recursive type {!type:t}, which can then finally be translated into a scad
    script. As this transpiles down to OpenSCAD, the
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language}
    User Manual} can be referred to for additional information about most of the
    functions made available here. *)

type t

(** {1 3d shape primitives} *)

(** [cube ?center dimensions] *)
val cube : ?center:bool -> float * float * float -> t

(** [sphere ?fa ?fs ?fn radius] *)
val sphere : ?fa:float -> ?fs:float -> ?fn:int -> float -> t

(** [cylinder ?center ?fa ?fs ?fn radius height] *)
val cylinder : ?center:bool -> ?fa:float -> ?fs:float -> ?fn:int -> float -> float -> t

(** [polyhedron points faces] *)
val polyhedron : Vec3.t list -> int list list -> t

(** {1 2d shape primitives} *)

(** [square ?center dimensions] *)
val square : ?center:bool -> float * float -> t

(** [circle ?fa ?fs ?fn radius] *)
val circle : ?fa:float -> ?fs:float -> ?fn:int -> float -> t

(** [polygon ?convexity ?paths points] *)
val polygon : ?convexity:int -> ?paths:int list list -> (float * float) list -> t

(** [text ?size ?font ?halign ?valign ?spacing ?direction ?language ?script ?fn str] *)
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
  -> t

(** {1 Transformations} *)

(** [translate p t] *)
val translate : Vec3.t -> t -> t

(** [rotate r t] *)
val rotate : Vec3.t -> t -> t

(** [rotate_about_pt r p t] *)
val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t

(** [vector_rotate ax r t] *)
val vector_rotate : Vec3.t -> float -> t -> t

(** [vector_rotate_about_pt ax r p t] *)
val vector_rotate_about_pt : Vec3.t -> float -> Vec3.t -> t -> t

(** [multmatrix mat t] *)
val multmatrix : MultMatrix.t -> t -> t

(** [mirror ax t] *)
val mirror : float * float * float -> t -> t

(** [quaternion q t] *)
val quaternion : Quaternion.t -> t -> t

(** [quaternion_about_pt q p t] *)
val quaternion_about_pt : Quaternion.t -> Vec3.t -> t -> t

(** [scale factors t]*)
val scale : float * float * float -> t -> t

(** [resize dimensions t]*)
val resize : float * float * float -> t -> t

(** [offset ?chamfer offset t ]*)
val offset : ?chamfer:bool -> [ `Delta of float | `Radius of float ] -> t -> t

(** [color ?alpha color t] *)
val color : ?alpha:float -> Color.t -> t -> t

(** {1 Boolean Combination} *)

(** [union ts] *)
val union : t list -> t

(** [minkowski ts] *)
val minkowski : t list -> t

(** [hull ts] *)
val hull : t list -> t

(** [difference t sub] *)
val difference : t -> t list -> t

(** [intersection ts] *)
val intersection : t list -> t

(** {1 3d to 2d} *)

(** [projection ?cut t] *)
val projection : ?cut:bool -> t -> t

(** {1 2d to 3d extrusions} *)

(** [linear_extrude ?height ?center ?convexity ?twist ?slices ?scale ?fn t]*)
val linear_extrude
  :  ?height:float
  -> ?center:bool
  -> ?convexity:int
  -> ?twist:int
  -> ?slices:int
  -> ?scale:float
  -> ?fn:int
  -> t
  -> t

(** [rotate_extrude ?angle ?convexity ?fa ?fs ?fn t]*)
val rotate_extrude
  :  ?angle:float
  -> ?convexity:int
  -> ?fa:float
  -> ?fs:float
  -> ?fn:int
  -> t
  -> t

(** [import ?dxf_layer ?convexity file] *)
val import : ?dxf_layer:string -> ?convexity:int -> string -> t

(** [t |>> p]

    Infix {!translate} *)
val ( |>> ) : t -> Vec3.t -> t

(** [t |@> r]

    Infix {!rotate} *)
val ( |@> ) : t -> Vec3.t -> t

(** [to_string t] *)
val to_string : t -> string

(** [write oc t]*)
val write : out_channel -> t -> unit
