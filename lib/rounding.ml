module type S = sig
  type vec
  type radius = [ `Radius of float ]
  type joint = [ `Joint of float ]
  type cut = [ `Cut of float ]
  type width = [ `Width of float ]

  type spec =
    | Chamf of { spec : [ joint | cut | width ] }
    | Circ of { spec : [ radius | joint | cut ] }
    | Bez of
        { spec : [ joint | cut ]
        ; curv : float option
        }

  type shape_spec =
    | Mix of (vec * spec option) list
    | Flat of
        { shape : vec list
        ; spec : spec
        ; closed : bool
        }

  val chamf : [ cut | joint | width ] -> spec
  val circ : [ cut | joint | radius ] -> spec
  val bez : ?curv:float -> [ cut | joint ] -> spec
  val chamfers : kind:[ `Cut | `Joint | `Width ] -> (vec * float) list -> shape_spec
  val circles : kind:[ `Radius | `Cut | `Joint ] -> (vec * float) list -> shape_spec
  val beziers : ?curv:float -> kind:[ `Cut | `Joint ] -> (vec * float) list -> shape_spec
  val mix : (vec * spec option) list -> shape_spec
  val flat : ?closed:bool -> spec:spec -> vec list -> shape_spec
  val corners : ?fn:int -> ?fa:float -> ?fs:float -> shape_spec -> vec list
end

module Make (V : Sigs.Vec) (Arc : Sigs.ArcProvider with type vec := V.t) = struct
  module Bz = Bezier.Make (V)
  module P = Path.Make (V)

  type radius = [ `Radius of float ]
  type joint = [ `Joint of float ]
  type cut = [ `Cut of float ]
  type width = [ `Width of float ]

  type spec =
    | Chamf of { spec : [ joint | cut | width ] }
    | Circ of { spec : [ radius | joint | cut ] }
    | Bez of
        { spec : [ joint | cut ]
        ; curv : float
        }

  type shape_spec =
    | Mix of (V.t * spec option) list
    | Flat of
        { shape : V.t list
        ; spec : spec
        ; closed : bool
        }

  let chamf spec = Chamf { spec }
  let circ spec = Circ { spec }
  let bez ?(curv = 0.5) spec = Bez { spec; curv }
  let mix ss = Mix ss
  let flat ?(closed = true) ~spec shape = Flat { shape; spec; closed }

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

  let smooth_bez_fill ~curv p1 p2 p3 =
    [ p1; V.lerp p1 p2 curv; p2; V.lerp p2 p3 curv; p3 ]

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
          [ add p2 (mul_scalar prev d)
          ; add p2 (mul_scalar prev (curv *. d))
          ; p2
          ; add p2 (mul_scalar next (curv *. d))
          ; add p2 (mul_scalar next d)
          ]
      | None   -> smooth_bez_fill ~curv p1 p2 p3
    in
    let fn =
      Int.max 3 (Option.value ~default:Float.(to_int @@ ceil (Bz.travel ps /. fs)) fn)
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
    V.[ add p2 (mul_scalar prev dist); add p2 (mul_scalar next dist) ]

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
    let p1' = V.(add p2 (mul_scalar prev dist))
    and p3' = V.(add p2 (mul_scalar next dist)) in
    if is_180
    then [ p1'; p3' ]
    else (
      let centre =
        V.(add p2 (mul_scalar (normalize @@ add prev next) (rad /. Float.sin half_angle)))
      and fn =
        let frags = Float.of_int @@ Util.helical_fragments ?fn ~fa ~fs rad in
        Float.(to_int @@ max 3. @@ ceil (((pi /. 2.) -. half_angle) /. pi *. frags))
      in
      Arc.arc_about_centre ~fn ~centre p1' p3' )

  let spec_to_corner ?fn ?fa ?fs t =
    match t with
    | Chamf { spec }     -> chamfer_corner ~spec
    | Circ { spec }      -> circle_corner ?fn ?fa ?fs ~spec
    | Bez { spec; curv } -> bez_corner ?fn ?fs ~curv ~spec

  let prune_mixed_spec mix =
    let shape, specs = Util.unzip mix in
    let shape = Array.of_list shape in
    let len = Array.length shape in
    let w = Util.index_wrap ~len in
    let f (i, pts, sps) sp =
      let p = shape.(i) in
      if (not (V.colinear shape.(w (i - 1)) p shape.(w (i + 1)))) || Option.is_none sp
      then i + 1, p :: pts, sp :: sps
      else i + 1, pts, sps
    in
    let _, shape, specs = List.fold_left f (0, [], []) specs in
    Util.array_of_list_rev shape, Array.get (Util.array_of_list_rev specs)

  let corners ?fn ?fa ?fs shape_spec =
    let shape, get_spec =
      match shape_spec with
      | Mix mix                      -> prune_mixed_spec mix
      | Flat { shape; spec; closed } ->
        let shape = P.prune_colinear' (Array.of_list shape) in
        let len = Array.length shape in
        let get_spec =
          if closed
          then fun _ -> Some spec
          else fun i -> if i = 0 || i = len - 1 then None else Some spec
        in
        shape, get_spec
    in
    let len = Array.length shape in
    let wrap = Util.index_wrap ~len in
    let f i =
      match get_spec i with
      | Some spec ->
        let corner = spec_to_corner ?fn ?fa ?fs spec in
        corner shape.(wrap (i - 1)) shape.(i) shape.(wrap (i + 1))
      | None      -> [ shape.(i) ]
    in
    List.init len f |> List.concat |> Util.deduplicate_consecutive ~equal:V.approx
end
