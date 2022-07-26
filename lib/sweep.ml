open Util
open Vec
module R2 = Rounding.Make (Vec2) (Arc2)

module Cap = struct
  type offset =
    { d : float
    ; z : float
    }

  type offset_mode =
    | Delta
    | Chamfer
    | Radius of
        { fn : int option
        ; fs : float option
        ; fa : float option
        }

  type offsets = Offsets : offset list -> offsets

  type holes =
    [ `Same
    | `Flip
    | `Spec of offsets
    | `Mix of [ `Same | `Flip | `Spec of offsets ] list
    ]

  type poly =
    { outer : offsets
    ; holes : holes
    ; mode : offset_mode
    }

  type poly_spec =
    [ `Empty
    | `Flat
    | `Round of poly
    ]

  type path_spec =
    [ `Empty
    | `Flat
    | `Round of offsets
    ]

  let poly_to_path_spec = function
    | `Flat               -> `Flat
    | `Empty              -> `Empty
    | `Round { outer; _ } -> `Round outer

  type caps =
    { top : poly_spec
    ; bot : poly_spec
    }

  type t =
    [ `Looped
    | `Caps of caps
    ]

  let delta_mode = Delta
  let chamfer_mode = Chamfer
  let radius_mode ?fn ?fs ?fa () = Radius { fn; fs; fa }
  let round ?(mode = Delta) ?(holes = `Flip) outer = `Round { outer; holes; mode }
  let looped = `Looped
  let capped ~top ~bot = `Caps { top; bot }
  let flat_caps = `Caps { top = `Flat; bot = `Flat }
  let open_caps = `Caps { top = `Empty; bot = `Empty }
  let quantize = Math.quant ~q:(1. /. 1024.)

  let radius_of_roundover = function
    | `Radius r -> r
    | `Cut c    -> c /. (Float.sqrt 2. -. 1.)

  let chamf ?(angle = Float.pi /. 4.) ?cut ?width ?height () =
    let a = Float.(min (pi /. 2.) (abs angle)) in
    let width, height =
      match width, height, cut with
      | _, _, Some c         -> c /. Float.cos a, c /. Float.sin a
      | Some w, Some h, None -> w, h
      | Some w, None, None   -> w, w /. Float.tan a
      | None, Some h, None   -> h *. Float.tan a, h
      | None, None, None     ->
        invalid_arg "At least one of cut, width, or height must be specified for chamfer."
    in
    Offsets [ { d = -.width; z = Float.abs height } ]

  let circ ?(fn = 16) roundover =
    let radius = radius_of_roundover roundover in
    let abs_radius = Float.abs radius
    and step = Float.pi /. 2. /. Float.of_int (Int.max 3 fn) in
    let f i (acc, last_z) =
      let i = Float.of_int (i + 1) in
      let z = quantize (abs_radius *. Float.sin (i *. step)) in
      if Math.approx last_z z
      then acc, last_z
      else { d = quantize (radius *. (Float.cos (i *. step) -. 1.)); z } :: acc, z
    in
    Offsets (List.rev @@ fst @@ Util.fold_init (fn + 1) f ([], Float.min_float))

  let tear ?(fn = 16) roundover =
    let radius = radius_of_roundover roundover in
    let abs_radius = Float.abs radius
    and step = Float.pi /. 4. /. Float.of_int (Int.max 3 (fn - 1)) in
    let f i (acc, last_z) =
      if i < fn
      then (
        let i = Float.of_int (i + 1) in
        let z = quantize (abs_radius *. Float.sin (i *. step)) in
        if Math.approx last_z z
        then acc, last_z
        else { d = quantize (radius *. (Float.cos (i *. step) -. 1.)); z } :: acc, z )
      else
        ( { d = -2. *. radius *. (1. -. (Float.sqrt 2. /. 2.)); z = abs_radius } :: acc
        , abs_radius )
    in
    Offsets (List.rev @@ fst @@ Util.fold_init (fn + 1) f ([], Float.min_float))

  let bez ?(curv = 0.5) ?(fn = 16) spec =
    let joint =
      match spec with
      | `Joint j -> j
      | `Cut c   -> 16. *. c /. Float.sqrt 2. /. (1. +. (4. *. curv))
    in
    let f (acc, last_z) { x = d; y = z } =
      let z = quantize z in
      if Math.approx last_z z then acc, last_z else { d = quantize d; z } :: acc, z
    in
    Offsets
      ( R2.bez_corner
          ~fn:(Int.max 1 fn + 2)
          ~curv
          ~spec:(`Joint joint)
          Vec2.zero
          (v2 0. (Float.abs joint))
          (v2 (-.joint) (Float.abs joint))
      |> List.tl
      |> List.fold_left f ([], Float.min_float)
      |> fst
      |> List.rev )

  let offsets offsets =
    let f (last_z, acc) { d; z } =
      let z = quantize @@ Float.abs z in
      if Float.compare z last_z <> 1
      then invalid_arg "Z offsets must increase monotonically."
      else z, { d = quantize d *. -1.; z } :: acc
    in
    let _, offsets = List.fold_left f (Float.min_float, []) offsets in
    Offsets (List.rev offsets)

  let unwrap_offsets (Offsets l) = l
  let flip_d (Offsets l) = Offsets (List.map (fun { d; z } -> { d = d *. -1.; z }) l)

  let unpack_poly_spec ?(n_holes = 0) spec =
    let hole_spec outer_offsets = function
      | `Same      -> fun _ -> `Round outer_offsets
      | `Flip      ->
        let flipped = flip_d outer_offsets in
        fun _ -> `Round flipped
      | `Spec offs -> fun _ -> `Round offs
      | `Mix specs ->
        let specs = Array.of_list specs in
        if Array.length specs = n_holes
        then
          fun i ->
          match Array.get specs i with
          | `Same      -> `Round outer_offsets
          | `Flip      -> `Round (flip_d outer_offsets)
          | `Spec offs -> `Round offs
        else invalid_arg "Mixed hole specs must match the number of holes."
    in
    match spec with
    | `Flat                         -> `Flat, (fun _ -> `Flat), None
    | `Empty                        -> `Empty, (fun _ -> `Empty), None
    | `Round { outer; holes; mode } -> `Round outer, hole_spec outer holes, Some mode
end

open Cap

let cap ?check_valid ?len ~flip ~close ~offset_mode ~m ~offsets shape =
  let len =
    match len with
    | Some l -> l
    | None   -> List.length shape
  and z_dir = if flip then -1. else 1.
  and lift ~z m = List.map (fun p -> MultMatrix.transform m @@ Vec2.to_vec3 ~z p) in
  let f (pts, faces, start_idx, last_shape, last_len, last_d) { d; z } =
    let mode, fn, fs, fa =
      match offset_mode with
      | Radius { fn; fs; fa } -> `Radius, fn, fs, fa
      | Delta                 -> `Delta, None, None, None
      | Chamfer               -> `Chamfer, None, None, None
    and z = z *. z_dir in
    let n, ps, fs =
      Offset.offset_with_faces
        ?check_valid
        ?fn
        ?fs
        ?fa
        ~flip_faces:flip
        ~start_idx
        ~mode
        (d -. last_d)
        last_shape
    in
    lift ~z m ps :: pts, fs :: faces, start_idx + last_len, ps, n, d
  in
  let points, faces, idx, last_shape, last_len, _ =
    List.fold_left f ([ lift ~z:0. m shape ], [], 0, shape, len, 0.) offsets
  in
  let faces =
    if close
    then (
      let close =
        if flip then fun i _ -> last_len + idx - i - 1 else fun i _ -> i + idx
      in
      List.mapi close last_shape :: List.concat faces )
    else List.concat faces
  in
  List.hd points, Mesh0.make ~points:List.(concat (rev points)) ~faces

let sweep'
    ?check_valid
    ?(winding = `CCW)
    ?(sealed = true)
    ?(top_mode = radius_mode ())
    ?(bot_mode = radius_mode ())
    ~top
    ~bot
    ~transforms
    shape
  =
  let shape = Mesh0.enforce_winding winding shape in
  let unpack_cap = function
    | `Flat                    -> sealed, []
    | `Empty                   -> false, []
    | `Round (Offsets offsets) -> sealed, offsets
  in
  let close_top, top_offsets = unpack_cap top
  and close_bot, bot_offsets = unpack_cap bot
  and len = List.length shape in
  let cap = function
    | `Top ->
      cap
        ?check_valid
        ~len
        ~flip:false
        ~close:close_top
        ~offset_mode:top_mode
        ~offsets:top_offsets
    | `Bot ->
      cap
        ?check_valid
        ~len
        ~flip:true
        ~close:close_bot
        ~offset_mode:bot_mode
        ~offsets:bot_offsets
  in
  match transforms with
  | []       ->
    let bot_lid, bot = cap `Bot ~m:MultMatrix.id shape
    and top_lid, top = cap `Top ~m:MultMatrix.id shape in
    bot_lid, top_lid, Mesh0.join [ bot; top ]
  | hd :: tl ->
    let mid, last_transform =
      let f (acc, _last) m =
        let lifted = List.map (fun p -> MultMatrix.transform m @@ Vec2.to_vec3 p) shape in
        lifted :: acc, m
      in
      List.fold_left f (f ([], hd) hd) tl
    in
    let mid = Mesh0.of_rows ~endcaps:`None (List.rev mid)
    and bot_lid, bot = cap `Bot ~m:hd shape
    and top_lid, top = cap `Top ~m:last_transform shape in
    bot_lid, top_lid, Mesh0.join [ bot; mid; top ]

let sweep
    ?check_valid
    ?(merge = true)
    ?winding
    ?(spec = flat_caps)
    ~transforms
    Poly2.{ outer; holes }
  =
  let sweep = sweep' ?check_valid ~transforms in
  let mesh =
    match spec, holes with
    | `Caps { top; bot }, []    ->
      let top = poly_to_path_spec top
      and bot = poly_to_path_spec bot in
      let _, _, poly = sweep ?winding ~top ~bot outer in
      poly
    | `Looped, holes            ->
      let f ~winding path =
        let path = Mesh0.enforce_winding winding path in
        List.map (fun m -> Path2.multmatrix m path) transforms
        |> Mesh0.of_rows ~endcaps:`Loop
      in
      Mesh0.join (f ~winding:`CCW outer :: List.map (f ~winding:`CW) holes)
    | `Caps { top; bot }, holes ->
      let n_holes = List.length holes in
      let top, top_holes, top_mode = unpack_poly_spec ~n_holes top
      and bot, bot_holes, bot_mode = unpack_poly_spec ~n_holes bot in
      let _, tunnel_bots, tunnel_tops, tunnels =
        let f (i, bots, tops, tuns) hole =
          let bot, top, tunnel =
            sweep
              ~winding:`CW
              ~sealed:false
              ?top_mode
              ?bot_mode
              ~top:(top_holes i)
              ~bot:(bot_holes i)
              hole
          in
          i + 1, bot :: bots, top :: tops, tunnel :: tuns
        in
        List.fold_left f (0, [], [], []) holes
      in
      let validate =
        match check_valid with
        | Some `No -> false
        | _        -> true
      and outer_bot, outer_top, outer =
        sweep ~winding:`CCW ~sealed:false ?top_mode ?bot_mode ~top ~bot outer
      in
      let bot_lid =
        Mesh0.of_poly3 ~rev:true (Poly3.make ~validate ~holes:tunnel_bots outer_bot)
      and top_lid = Mesh0.of_poly3 (Poly3.make ~validate ~holes:tunnel_tops outer_top) in
      Mesh0.join (bot_lid :: top_lid :: outer :: tunnels)
  in
  if merge then Mesh0.merge_points mesh else mesh

let linear_extrude
    ?check_valid
    ?merge
    ?winding
    ?fa
    ?slices
    ?scale_k
    ?twist_k
    ?scale
    ?(twist = 0.)
    ?(center = false)
    ?(caps = { top = `Flat; bot = `Flat })
    ~height
    shape
  =
  let slices = helical_slices ?fa ?fn:slices twist in
  let cap_height = function
    | `Flat | `Empty -> 0.
    | `Round { outer = Offsets l; _ } -> List.fold_left (fun _ { z; _ } -> z) 0. l
  in
  let bot_height = cap_height caps.bot
  and top_height = cap_height caps.top in
  let z = if center then height /. -2. else bot_height
  and s = Float.max 0. (height -. bot_height -. top_height) /. Float.of_int slices
  and twist = if Float.abs twist > 0. then Some twist else None in
  let transforms =
    List.init (slices + 1) (fun i -> v3 0. 0. ((Float.of_int i *. s) +. z))
    |> Path3.to_transforms ?scale_k ?twist_k ?scale ?twist
  in
  sweep ?check_valid ?merge ?winding ~spec:(`Caps caps) ~transforms shape

let helix_extrude
    ?check_valid
    ?merge
    ?fn
    ?fa
    ?fs
    ?scale_k
    ?twist_k
    ?scale
    ?twist
    ?(caps = { top = `Flat; bot = `Flat })
    ?(left = true)
    ~n_turns
    ~pitch
    ?r2
    r1
    shape
  =
  let transforms =
    Path3.helical_transforms
      ?fn
      ?fa
      ?fs
      ?scale_k
      ?twist_k
      ?scale
      ?twist
      ~n_turns
      ~pitch
      ?r2
      r1
  and winding = if left then `CCW else `CW in
  sweep ?check_valid ?merge ~winding ~spec:(`Caps caps) ~transforms shape

let path_extrude
    ?check_valid
    ?merge
    ?winding
    ?spec
    ?euler
    ?scale_k
    ?twist_k
    ?scale
    ?twist
    ~path
  =
  let transforms = Path3.to_transforms ?euler ?scale_k ?twist_k ?scale ?twist path in
  sweep ?check_valid ?merge ?winding ?spec ~transforms
