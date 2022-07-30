type resampler =
  [ `Direct of [ `ByLen | `BySeg ]
  | `Reindex of [ `ByLen | `BySeg ]
  ]

type duplicator =
  [ `Distance
  | `FastDistance
  | `Tangent
  ]

type mapping =
  [ resampler
  | duplicator
  ]

(** [slice_profiles]

*)

val slice_profiles
  :  ?looped:bool
  -> slices:[< `Flat of int | `Mix of int list ]
  -> Path3.t list
  -> Path3.t list

(** [distance_match]

*)
val distance_match : Path3.t -> Path3.t -> Path3.t * Path3.t

(** [aligned_distance_match]

*)
val aligned_distance_match : Path3.t -> Path3.t -> Path3.t * Path3.t

(** [tangent_match]

*)
val tangent_match : Path3.t -> Path3.t -> Path3.t * Path3.t

(** [skin]

*)
val skin
  :  ?style:Mesh0.style
  -> ?endcaps:Mesh0.endcaps
  -> ?refine:int
  -> ?mapping:[ `Flat of mapping | `Mix of mapping list ]
  -> slices:[< `Flat of int | `Mix of int list ]
  -> Path3.t list
  -> Mesh0.t

(** [morph_between]

*)
val morph_between
  :  ?style:Mesh0.style
  -> ?endcaps:Mesh0.endcaps
  -> ?refine:int
  -> ?mapping:mapping
  -> slices:int
  -> Path3.t
  -> Path3.t
  -> Mesh0.t
