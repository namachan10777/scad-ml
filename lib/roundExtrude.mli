type offset =
  { d : float
  ; z : float
  }

(* type spec *)

(* type hole_spec = *)
(*   [ `Same *)
(*   | `Flip *)
(*   | `Custom of spec *)
(*   ] *)

(* type hole = *)
(*   { hole : Vec2.t list *)
(*   ; top_spec : hole_spec option *)
(*   ; bot_spec : hole_spec option *)
(*   } *)

type offsets

type hole_spec =
  [ `Same
  | `Flip
  | `Custom of offsets
  | `Mix of [ `Same | `Flip | `Custom of offsets ] list
  ]

type poly_spec =
  { outer : offsets
  ; holes : hole_spec
  }

type cap_spec =
  [ `Empty
  | `Flat
  | `Round of poly_spec
  ]

type path_spec =
  [ `Empty
  | `Flat
  | `Round of offsets
  ]

type caps =
  { top : cap_spec
  ; bot : cap_spec
  }

type spec =
  [ `Looped
  | `Caps of caps
  ]

val chamf : ?angle:float -> ?cut:float -> ?width:float -> ?height:float -> unit -> offsets
val circ : ?fn:int -> [< `Cut of float | `Radius of float ] -> offsets
val tear : ?fn:int -> [< `Cut of float | `Radius of float ] -> offsets
val bez : ?curv:float -> ?fn:int -> [< `Cut of float | `Joint of float ] -> offsets
val custom : offset list -> offsets
val round : ?holes:hole_spec -> offsets -> [> `Round of poly_spec ]
val looped : spec
val capped : top:cap_spec -> bot:cap_spec -> spec
val flat_caps : spec
val open_caps : spec
(* val hole : ?bot:hole_spec option -> ?top:hole_spec option -> Vec2.t list -> hole *)

(* val sweep *)
(*   :  ?check_valid:int option *)
(*   -> ?winding:[ `CCW | `CW | `NoCheck ] *)
(*   -> ?fn:int *)
(*   -> ?fs:float *)
(*   -> ?fa:float *)
(*   -> ?mode:[ `Chamfer | `Delta | `Radius ] *)
(*   -> ?caps:[ `Capped | `Open ] *)
(*   -> ?top:spec *)
(*   -> ?bot:spec *)
(*   -> ?holes:hole list *)
(*   -> transforms:MultMatrix.t list *)
(*   -> Vec2.t list *)
(*   -> Mesh.t *)

val sweep
  :  ?check_valid:int option
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?mode:[< `Chamfer | `Delta | `Radius > `Radius ]
  -> ?spec:[< `Caps of caps | `Looped > `Caps ]
  -> transforms:MultMatrix.t list
  -> Poly2.t
  -> Mesh.t

(* val linear_extrude *)
(*   :  ?check_valid:int option *)
(*   -> ?winding:[ `CCW | `CW | `NoCheck ] *)
(*   -> ?fn:int *)
(*   -> ?fs:float *)
(*   -> ?fa:float *)
(*   -> ?slices:int *)
(*   -> ?scale:Vec2.t *)
(*   -> ?twist:float *)
(*   -> ?center:bool *)
(*   -> ?mode:[< `Chamfer | `Delta | `Radius > `Radius ] *)
(*   -> ?caps:[ `Capped | `Open ] *)
(*   -> ?top:spec *)
(*   -> ?bot:spec *)
(*   -> ?holes:hole list *)
(*   -> height:float *)
(*   -> Vec2.t list *)
(*   -> Mesh.t *)

val linear_extrude
  :  ?check_valid:int option
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?slices:int
  -> ?scale:Vec2.t
  -> ?twist:float
  -> ?center:bool
  -> ?mode:[< `Chamfer | `Delta | `Radius > `Radius ]
  -> ?caps:caps
  -> height:float
  -> Poly2.t
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
