(** Path resampling vertex mapping strategies.

    Each of these variants specify that profiles of incommensurate length should
    simply resampled with {!Path3.subdivide} with the provided point distribution
    frequency strategy ([[`ByLen | `BySeg]]). In the case of [`Direct _], the
    profiles are assumed to be "lined up", with the points at their zeroth indices
    corresponding to eachother. The [`Reindex _] strategy will rotate the
    second profile of a pair via {!Path3.reindex_polygon} following resampling
    to minimize the distance between the zeroth indices of the two paths. *)
type resampler =
  [ `Direct of [ `ByLen | `BySeg ]
  | `Reindex of [ `ByLen | `BySeg ]
  ]

(** Point duplicating vertex mapping strategies.

    Each of these variants specify profiles of incommensurate length should be
    matched up by computing vertex mappings between the profiles, and
    duplicating vertices on the smaller/shorter profile until the point counts are
    equalized. See the conspicuously named implementation functions
    {!distance_match}, {!aligned_distance_match}, and {!tangent_match} for more
    details. *)
type duplicator =
  [ `Distance (** minimize the length of the edges between associated vertices *)
  | `FastDistance
    (** like [`Distance], but profiles are assumed to already be
          lined up, with their zeroth indices corresponding to one another *)
  | `Tangent
    (** split finely sampled (convex) curve into groups of points, and
          map each group to point on the smaller discrete polygon *)
  ]

(** Vertex count matching strategy specification type. *)
type mapping =
  [ resampler
  | duplicator
  ]

(** [slice_profiles ?looped ~slices profiles]

    Linearly transition between each neighbouring pair of closed paths in
    [profiles] to produce new interpolated list of profiles. The number of
    [slices] inserted between can either be the same between each pair
    ([`Flat n]), or specified separately with [`Mix ns]. If [looped] is [true],
    then slices will also be inserted between the last and initial profiles
    (default is [false]).

    Raises [Invalid_argument] if the length of [`Mix ns] does not correspond to
    the number of transitions, or if [profiles] has fewer than two elements. *)
val slice_profiles
  :  ?looped:bool
  -> slices:[< `Flat of int | `Mix of int list ]
  -> Path3.t list
  -> Path3.t list

(** [distance_match a b]

*)
val distance_match : Path3.t -> Path3.t -> Path3.t * Path3.t

(** [aligned_distance_match a b]

*)
val aligned_distance_match : Path3.t -> Path3.t -> Path3.t * Path3.t

(** [tangent_match a b]

*)
val tangent_match : Path3.t -> Path3.t -> Path3.t * Path3.t

(** [skin ?style ?endcaps ?refine ?mapping ~slices profiles]

*)
val skin
  :  ?style:Mesh0.style
  -> ?endcaps:Mesh0.endcaps
  -> ?refine:int
  -> ?mapping:[ `Flat of mapping | `Mix of mapping list ]
  -> slices:[< `Flat of int | `Mix of int list ]
  -> Path3.t list
  -> Mesh0.t

(** [skin_between ?style ?endcaps ?refine ?mapping ~slices a b]

*)
val skin_between
  :  ?style:Mesh0.style
  -> ?endcaps:Mesh0.endcaps
  -> ?refine:int
  -> ?mapping:mapping
  -> slices:int
  -> Path3.t
  -> Path3.t
  -> Mesh0.t
