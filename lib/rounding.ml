module type S = sig
  (** Roundovers inspired by the {{:https://github.com/revarbat/BOSL2} BOSL2}
   {{:https://github.com/revarbat/BOSL2/blob/master/rounding.scad} rounding} module. *)

  type vec

  (** Configuration module with types and helpers for specifying path
    roundovers. *)
  module Round : sig
    (** Radius of circular arc roundovers. *)
    type radius = [ `Radius of float ]

    (** Distance away from the corner the roundover should start. *)
    type joint = [ `Joint of float ]

    (** Distance in from the corner that should be cut off by the roundover. *)
    type cut = [ `Cut of float ]

    (** Width of the segment replacing chamfered corners. *)
    type width = [ `Width of float ]

    (** Roundover specification for a corner of a path. *)
    type corner =
      | Chamf of [ joint | cut | width ] (** corner chamfer *)
      | Circ of [ radius | joint | cut ] (** circular roundover *)
      | Bez of
          { spec : [ joint | cut ] (** continuous curvature roundover *)
          ; curv : float (** bezier curvature smoothness *)
          }

    (** Full roundover specification for a path, either given as a mixed list of
         pairs of coordinates and {!type:corner} specifications that apply to them,
         or a single spec to be applied to all corners of the included path. *)
    type t =
      | Mix of (vec * corner option) list
      | Flat of
          { path : vec list
          ; corner : corner
          ; closed : bool
                (** If [true], roundover will be applied on the first
                        and last points, otherwise they will be left untouched. *)
          }

    (** [chamf spec]

        Create a chamfered {!type:corner} specification. *)
    val chamf : [ cut | joint | width ] -> corner

    (** [circ spec]

        Create a circular {!type:corner} specification. *)
    val circ : [ cut | joint | radius ] -> corner

    (** [bez ?curv spec]

        Create a continuous curvature {!type:corner} specification. [curv] sets
        the smoothness of bezier curvature (default = [0.5]). *)
    val bez : ?curv:float -> [ cut | joint ] -> corner

    (** [mix l]

        Wrap a list of point * optional corner specification pairs as a
        {!type:t}. Note that it is the users responsibility to leave the specs for
        the first and last points as [None] if they intend to treat the path as
        open. *)
    val mix : (vec * corner option) list -> t

    (** [flat ?closed ~corner path]

        Create a roundover specification that will apply [corner] to each of
        the points in [path] (other than the first and last points if [closed] is
        [false], default = [true]). *)
    val flat : ?closed:bool -> corner:corner -> vec list -> t

    (** [chamfers ~kind spec_pts]

        Create an all chamfer {!type:t} specification, with variable amplitude
        of the given [kind] paired with each point of the path. *)
    val chamfers : kind:[ `Cut | `Joint | `Width ] -> (vec * float) list -> t

    (** [circles ~kind spec_pts]

        Create an all circular {!type:t} specification, with variable amplitude
        of the given [kind] paired with each point of the path. *)
    val circles : kind:[ `Radius | `Cut | `Joint ] -> (vec * float) list -> t

    (** [bezier ?curv ~kind spec_pts]

         Create an all continuour curvature {!type:t} specification, with variable
         amplitude of the given [kind] paired with each point of the path. Curvature
         smoothness of all roundovers is set by [curv] (default = [0.5]). If variable
         smoothness is desired, {!val:bez} and {!val:mix} may be used in conjunction
         to achieve it. *)
    val beziers : ?curv:float -> kind:[ `Cut | `Joint ] -> (vec * float) list -> t
  end

  (** [roundover ?fn ?fa ?fs path_spec]

      Apply the roundover specifictions in [path_spec] on the bundled
      path/shape, with quality set by the [fn], [fa], and [fs] parameters.
      Collinear points are ignored (included in output without roundover applied). *)
  val roundover : ?fn:int -> ?fa:float -> ?fs:float -> Round.t -> vec list
end

module type Arc = sig
  type vec

  val arc_about_centre
    :  ?rev:bool
    -> ?fn:int
    -> ?fa:float
    -> ?fs:float
    -> ?dir:[ `CW | `CCW ]
    -> ?wedge:bool
    -> centre:vec
    -> vec
    -> vec
    -> vec list
end

module Make (V : V.S) (Arc : Arc with type vec := V.t) = struct
  module Bz = Bezier.Make (V)
  module P = Path.Make (V)

  module Round = struct
    type radius = [ `Radius of float ]
    type joint = [ `Joint of float ]
    type cut = [ `Cut of float ]
    type width = [ `Width of float ]

    type corner =
      | Chamf of [ joint | cut | width ]
      | Circ of [ radius | joint | cut ]
      | Bez of
          { spec : [ joint | cut ]
          ; curv : float
          }

    type t =
      | Mix of (V.t * corner option) list
      | Flat of
          { path : V.t list
          ; corner : corner
          ; closed : bool
          }

    let chamf spec = Chamf spec
    let circ spec = Circ spec
    let bez ?(curv = 0.5) spec = Bez { spec; curv }
    let mix ss = Mix ss
    let flat ?(closed = true) ~corner path = Flat { path; corner; closed }

    let chamfers ~kind spec_pts =
      let wrap =
        match kind with
        | `Cut   -> fun c -> `Cut c
        | `Joint -> fun j -> `Joint j
        | `Width -> fun w -> `Width w
      in
      let f (p, v) = p, if Float.equal 0. v then None else Some (chamf (wrap v)) in
      mix @@ List.map f spec_pts

    let circles ~kind spec_pts =
      let wrap =
        match kind with
        | `Radius -> fun r -> `Radius r
        | `Cut    -> fun c -> `Cut c
        | `Joint  -> fun j -> `Joint j
      in
      let f (p, v) = p, if Float.equal 0. v then None else Some (circ (wrap v)) in
      mix @@ List.map f spec_pts

    let beziers ?curv ~kind spec_pts =
      let wrap =
        match kind with
        | `Cut   -> fun c -> `Cut c
        | `Joint -> fun j -> `Joint j
      in
      let f (p, v) = p, if Float.equal 0. v then None else Some (bez ?curv (wrap v)) in
      mix @@ List.map f spec_pts
  end

  open Round

  let smooth_bez_fill ~curv p1 p2 p3 =
    [ p1; V.lerp p2 p1 curv; p2; V.lerp p2 p3 curv; p3 ]

  let bez_corner ?fn ?(fs = Util.fs) ?(curv = 0.5) ?spec p1 p2 p3 =
    let ps =
      match spec with
      | Some s ->
        let d =
          match s with
          | `Joint d -> d
          | `Cut c   ->
            let half_angle = V.angle_points p1 p2 p3 /. 2. in
            8. *. c /. Float.cos half_angle /. (1. +. (4. *. curv))
        and prev = V.(normalize (sub p1 p2))
        and next = V.(normalize (sub p3 p2)) in
        V.
          [ add p2 (smul prev d)
          ; add p2 (smul prev (curv *. d))
          ; p2
          ; add p2 (smul next (curv *. d))
          ; add p2 (smul next d)
          ]
      | None   -> smooth_bez_fill ~curv p1 p2 p3
    in
    let fn =
      Int.max 3 (Option.value ~default:Float.(to_int @@ ceil (Bz.length ps /. fs)) fn)
    in
    Bz.curve ~fn (Bz.make ps)

  let chamfer_corner ~spec p1 p2 p3 =
    let dist =
      match spec with
      | `Joint d -> d
      | `Cut c   -> c /. Float.cos (V.angle_points p1 p2 p3 /. 2.)
      | `Width w -> w /. Float.sin (V.angle_points p1 p2 p3 /. 2.) /. 2.
    and prev = V.(normalize (sub p1 p2))
    and next = V.(normalize (sub p3 p2)) in
    V.[ add p2 (smul prev dist); add p2 (smul next dist) ]

  let circle_corner ?fn ?(fa = Util.fa) ?(fs = Util.fs) ~spec p1 p2 p3 =
    let half_angle = V.angle_points p1 p2 p3 /. 2. in
    let is_180 = Math.approx half_angle (Float.pi /. 2.) in
    let dist, rad =
      match spec with
      | `Joint d  ->
        let rad = d *. Float.tan half_angle in
        d, rad
      | `Radius r -> r /. Float.tan half_angle, r
      | `Cut c    ->
        let rad = c /. ((1. /. Float.sin half_angle) -. 1.) in
        (if is_180 then Float.infinity else rad /. Float.tan half_angle), rad
    in
    let prev = V.(normalize (sub p1 p2))
    and next = V.(normalize (sub p3 p2)) in
    let p1' = V.(add p2 (smul prev dist))
    and p3' = V.(add p2 (smul next dist)) in
    if is_180
    then [ p1'; p3' ]
    else (
      let centre =
        V.(add p2 (smul (normalize @@ add prev next) (rad /. Float.sin half_angle)))
      and fn =
        let frags = Float.of_int @@ Util.helical_fragments ?fn ~fa ~fs rad in
        Float.(to_int @@ max 3. @@ ceil (((pi /. 2.) -. half_angle) /. pi *. frags))
      in
      Arc.arc_about_centre ~fn ~centre p1' p3' )

  let spec_to_corner ?fn ?fa ?fs t =
    match t with
    | Chamf spec         -> chamfer_corner ~spec
    | Circ spec          -> circle_corner ?fn ?fa ?fs ~spec
    | Bez { spec; curv } -> bez_corner ?fn ?fs ~curv ~spec

  let prune_mixed_spec mix =
    let path, specs = Util.unzip mix in
    let path = Array.of_list path in
    let len = Array.length path in
    let w = Util.index_wrap ~len in
    let f (i, sps) sp =
      let p = path.(i) in
      if (not (V.collinear path.(w (i - 1)) p path.(w (i + 1)))) || Option.is_none sp
      then i + 1, sp :: sps
      else i + 1, None :: sps
    in
    let _, specs = List.fold_left f (0, []) specs in
    path, Array.get (Util.array_of_list_rev specs)

  let roundover ?fn ?fa ?fs path_spec =
    let path, get_spec =
      match path_spec with
      | Mix mix                       -> prune_mixed_spec mix
      | Flat { path; corner; closed } ->
        let path = Array.of_list path in
        let len = Array.length path in
        let get_corner =
          let w = Util.index_wrap ~len in
          let g i =
            if V.collinear path.(w (i - 1)) path.(i) path.(w (i + 1))
            then None
            else Some corner
          in
          if closed then g else fun i -> if i = 0 || i = len - 1 then None else g i
        in
        path, get_corner
    in
    let len = Array.length path in
    let wrap = Util.index_wrap ~len in
    let f i =
      match get_spec i with
      | Some spec ->
        let corner = spec_to_corner ?fn ?fa ?fs spec in
        corner path.(wrap (i - 1)) path.(i) path.(wrap (i + 1))
      | None      -> [ path.(i) ]
    in
    List.init len f |> List.concat |> P.deduplicate_consecutive
end
