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

val slice_profiles
  :  ?looped:bool
  -> slices:[< `Flat of int | `Mix of int list ]
  -> Path3.t list
  -> Path3.t list

val distance_match : Path3.t -> Path3.t -> Path3.t * Path3.t
val aligned_distance_match : Path3.t -> Path3.t -> Path3.t * Path3.t
val tangent_match : Path3.t -> Path3.t -> Path3.t * Path3.t

val skin
  :  ?style:Mesh0.style
  -> ?refine:int
  -> ?mapping:[ `Flat of mapping | `Mix of mapping list ]
  -> ?endcaps:Mesh0.endcaps
  -> slices:[< `Flat of int | `Mix of int list ]
  -> Path3.t list
  -> Mesh0.t

val morph_between
  :  ?style:Mesh0.style
  -> ?refine:int
  -> ?mapping:mapping
  -> ?endcaps:Mesh0.endcaps
  -> slices:[< `Flat of int | `Mix of int list ]
  -> Path3.t
  -> Path3.t
  -> Mesh0.t
