type spec

val chamf : ?angle:float -> ?cut:float -> ?width:float -> ?height:float -> unit -> spec
val circ : ?fn:int -> [< `Cut of float | `Radius of float ] -> spec
val tear : ?fn:int -> [< `Cut of float | `Radius of float ] -> spec
val bez : ?curv:float -> ?fn:int -> [< `Cut of float | `Joint of float ] -> spec
val custom : (float * float) list -> spec

val sweep
  :  ?check_valid:int option
  -> ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?mode:[< `Chamfer | `Delta | `Radius > `Radius ]
  -> ?top:spec
  -> ?bot:spec
  -> transforms:MultMatrix.t list
  -> Vec2.t list
  -> Poly3d.t

val linear_extrude
  :  ?check_valid:int option
  -> ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?slices:int
  -> ?scale:Vec2.t
  -> ?twist:float
  -> ?center:bool
  -> ?mode:[< `Chamfer | `Delta | `Radius > `Radius ]
  -> ?top:spec
  -> ?bot:spec
  -> height:float
  -> Vec2.t list
  -> Poly3d.t
