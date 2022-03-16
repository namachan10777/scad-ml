type offset =
  { d : float
  ; z : float
  }

type spec

type hole_spec =
  [ `Same
  | `Flip
  | `Custom of spec
  ]

type hole =
  { hole : Vec2.t list
  ; top_spec : hole_spec option
  ; bot_spec : hole_spec option
  }

val chamf : ?angle:float -> ?cut:float -> ?width:float -> ?height:float -> unit -> spec
val circ : ?fn:int -> [< `Cut of float | `Radius of float ] -> spec
val tear : ?fn:int -> [< `Cut of float | `Radius of float ] -> spec
val bez : ?curv:float -> ?fn:int -> [< `Cut of float | `Joint of float ] -> spec
val custom : offset list -> spec
val hole : ?bot:hole_spec option -> ?top:hole_spec option -> Vec2.t list -> hole

val sweep
  :  ?check_valid:int option
  -> ?winding:[ `CCW | `CW | `NoCheck ]
  -> ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?mode:[ `Chamfer | `Delta | `Radius ]
  -> ?caps:[ `Capped | `Open ]
  -> ?top:spec
  -> ?bot:spec
  -> ?holes:hole list
  -> transforms:MultMatrix.t list
  -> Vec2.t list
  -> Mesh.t

val linear_extrude
  :  ?check_valid:int option
  -> ?winding:[ `CCW | `CW | `NoCheck ]
  -> ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?slices:int
  -> ?scale:Vec2.t
  -> ?twist:float
  -> ?center:bool
  -> ?mode:[< `Chamfer | `Delta | `Radius > `Radius ]
  -> ?caps:[ `Capped | `Open ]
  -> ?top:spec
  -> ?bot:spec
  -> ?holes:hole list
  -> height:float
  -> Vec2.t list
  -> Mesh.t

val prism
  :  ?debug:bool
  -> ?fn:int
  -> ?k:float
  -> ?k_bot:float
  -> ?k_top:float
  -> ?k_sides:[< `Flat of float | `Mix of float list > `Flat ]
  -> ?joint_bot:float * float
  -> ?joint_top:float * float
  -> ?joint_sides:[< `Flat of float * float | `Mix of (float * float) list > `Flat ]
  -> Vec3.t list
  -> Vec3.t list
  -> Mesh.t
