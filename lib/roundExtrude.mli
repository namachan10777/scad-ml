(* TODO: Decide
   - delete this interface, anything exposed will be in Mesh as re-export
   - OR keep the interface, and just include into Mesh *)

module Spec : sig
  type offset =
    { d : float
    ; z : float
    }

  type offsets

  type holes =
    [ `Same
    | `Flip
    | `Custom of offsets
    | `Mix of [ `Same | `Flip | `Custom of offsets ] list
    ]

  type poly =
    { outer : offsets
    ; holes : holes
    }

  type cap =
    [ `Empty
    | `Flat
    | `Round of poly
    ]

  type path =
    [ `Empty
    | `Flat
    | `Round of offsets
    ]

  type caps =
    { top : cap
    ; bot : cap
    }

  type t =
    [ `Looped
    | `Caps of caps
    ]

  val chamf
    :  ?angle:float
    -> ?cut:float
    -> ?width:float
    -> ?height:float
    -> unit
    -> offsets

  val circ : ?fn:int -> [< `Cut of float | `Radius of float ] -> offsets
  val tear : ?fn:int -> [< `Cut of float | `Radius of float ] -> offsets
  val bez : ?curv:float -> ?fn:int -> [< `Cut of float | `Joint of float ] -> offsets
  val custom : offset list -> offsets
  val round : ?holes:holes -> offsets -> [> `Round of poly ]
  val looped : t
  val capped : top:cap -> bot:cap -> t
  val flat_caps : t
  val open_caps : t
end

val sweep
  :  ?check_valid:int option
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?mode:[< `Chamfer | `Delta | `Radius > `Radius ]
  -> ?spec:Spec.t
  -> transforms:MultMatrix.t list
  -> Poly2.t
  -> Mesh0.t

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
  -> ?caps:Spec.caps
  -> height:float
  -> Poly2.t
  -> Mesh0.t

val helix_extrude
  :  ?fn:int
  -> ?fa:float
  -> ?fs:float
  -> ?scale:Vec2.t
  -> ?twist:float
  -> ?mode:[< `Chamfer | `Delta | `Radius > `Radius ]
  -> ?caps:Spec.caps
  -> ?left:bool
  -> n_turns:int
  -> pitch:float
  -> ?r2:float
  -> float
  -> Poly2.t
  -> Mesh0.t

val path_extrude
  :  ?check_valid:int option
  -> ?winding:[< `CCW | `CW | `NoCheck > `CCW `CW ]
  -> ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?mode:[< `Chamfer | `Delta | `Radius > `Radius ]
  -> ?spec:Spec.t
  -> ?euler:bool
  -> ?scale:Vec2.t
  -> ?twist:float
  -> path:Path3.t
  -> Poly2.t
  -> Mesh0.t
