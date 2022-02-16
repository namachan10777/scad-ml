(* placeholder module, will likely have this stuff in poly3d as rounded_extrude *)
module R = Rounding.Make (Vec2) (Path2d)

type spec = Spec : (float * float) list -> spec

let quantize = Math.quant ~q:(1. /. 1024.)

let radius_of_spec = function
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
  Spec [ -.width, Float.abs height ]

let circ ?(fn = 16) spec =
  let radius = radius_of_spec spec in
  let step = Float.pi /. 2. /. Float.of_int (Int.max 3 fn) in
  let f i =
    let i = Float.of_int (i + 1) in
    ( quantize (radius *. (Float.cos (i *. step) -. 1.))
    , quantize (Float.abs radius *. Float.sin (i *. step)) )
  in
  Spec (List.init fn f)

let tear ?(fn = 16) spec =
  let radius = radius_of_spec spec in
  let step = Float.pi /. 4. /. Float.of_int (Int.max 3 fn) in
  let f i =
    if i < fn
    then (
      let i = Float.of_int (i + 1) in
      ( quantize (radius *. (Float.cos (i *. step) -. 1.))
      , quantize (Float.abs radius *. Float.sin (i *. step)) ) )
    else -2. *. radius *. (1. -. (Float.sqrt 2. /. 2.)), Float.abs radius
  in
  Spec (List.init (fn + 1) f)

let bez ?(curv = 0.5) ?(fn = 16) spec =
  let joint =
    match spec with
    | `Joint j -> j
    | `Cut c   -> 16. *. c /. Float.sqrt 2. /. (1. +. (4. *. curv))
  in
  Spec
    ( R.bez_corner
        ~fn:(Int.max 1 fn + 2)
        ~curv
        ~spec:(`Joint joint)
        (0., 0.)
        (0., Float.abs joint)
        (-.joint, Float.abs joint)
    |> List.tl
    |> List.map (fun (d, z) -> quantize d, quantize z) )

let custom l =
  Spec (List.map (fun (d, z) -> quantize d *. -1., quantize @@ Float.abs z) l)

let sweep'
    ?check_valid
    ?(winding = `CCW)
    ?fn
    ?fs
    ?fa
    ?(mode = `Radius)
    ?(caps = `Capped)
    ?top
    ?bot
    ~transforms
    shape
  =
  let shape =
    let reverse =
      match winding with
      | `CCW     -> Path2d.is_clockwise shape
      | `CW      -> not @@ Path2d.is_clockwise shape
      | `NoCheck -> false
    in
    if reverse then List.rev shape else shape
  in
  let (Spec top) = Option.value ~default:(Spec []) top
  and (Spec bot) = Option.value ~default:(Spec []) bot
  and len = List.length shape
  and offset = Offset2d.offset_with_faces ?check_valid ?fn ?fs ?fa
  and lift ?z m = List.map (fun p -> MultMatrix.transform m @@ Vec2.to_vec3 ?z p) in
  let cap ~top ~m offsets =
    let z_dir, flip_faces = if top then 1., false else -1., true in
    let f (pts, faces, start_idx, last_shape, last_len, last_d) (d, z) =
      let spec =
        match mode with
        | `Radius  -> `Radius (d -. last_d)
        | `Delta   -> `Delta (d -. last_d)
        | `Chamfer -> `Chamfer (d -. last_d)
      and z = z *. z_dir in
      let n, ps, fs = offset ~flip_faces ~start_idx spec last_shape in
      lift ~z m ps :: pts, fs :: faces, start_idx + last_len, ps, n, d
    in
    let points, faces, idx, last_shape, last_len, _ =
      List.fold_left f ([ lift m shape ], [], 0, shape, len, 0.) offsets
    in
    let faces =
      match caps with
      | `Capped ->
        let close =
          if flip_faces then fun i _ -> last_len + idx - i - 1 else fun i _ -> i + idx
        in
        List.mapi close last_shape :: List.concat faces
      | `Open   -> List.concat faces
    in
    last_shape, Poly3d.make ~points:List.(concat (rev points)) ~faces
  in
  match transforms with
  | []      ->
    let bot_lid, bot = cap ~top:false ~m:MultMatrix.id bot
    and top_lid, top = cap ~top:true ~m:MultMatrix.id top in
    bot_lid, top_lid, Poly3d.join [ bot; top ]
  | hd :: _ ->
    let mid, last_transform =
      let f (acc, _last) m = lift m shape :: acc, m in
      List.fold_left f ([], hd) transforms
    in
    let mid = Poly3d.of_layers ~caps:`Open (List.rev mid)
    and bot_lid, bot = cap ~top:false ~m:hd bot
    and top_lid, top = cap ~top:true ~m:last_transform top in
    bot_lid, top_lid, Poly3d.join [ bot; mid; top ]

(* NOTE: This is broken out to facilitate the code required for sweeping with
    holes. The bottom and top lid shapes will be used to generate faces with
    holes to seal the polyhedrons when internal shapes are given. Otherwise,
    they will just be ignored. *)
let sweep ?check_valid ?winding ?fn ?fs ?fa ?mode ?caps ?top ?bot ~transforms shape =
  (* TODO: instead of the 2d shapes of the lids, return the lifted ones?
     -> plane projection
     -> polyholes partition
     -> lift points back onto the plane
     -> Poly3d.join to slap them all together
     -> note that there will be duplicated points, so I'll probably want to
        trim those out? After I have it, add an option to do it in house? *)
  let _, _, poly =
    sweep' ?check_valid ?winding ?fn ?fs ?fa ?mode ?caps ?top ?bot ~transforms shape
  in
  poly

let linear_extrude
    ?check_valid
    ?winding
    ?fn
    ?fs
    ?fa
    ?slices
    ?scale
    ?(twist = 0.)
    ?(center = false)
    ?mode
    ?caps
    ?top
    ?bot
    ~height
    shape
  =
  let slices = Util.helical_slices ?fa ?fn:slices twist
  and (Spec top_spec) = Option.value ~default:(Spec []) top
  and (Spec bot_spec) = Option.value ~default:(Spec []) bot in
  let bot_height = List.fold_left (fun _ (_, z) -> z) 0. bot_spec
  and top_height = List.fold_left (fun _ (_, z) -> z) 0. top_spec in
  let z = if center then height /. -2. else bot_height
  and s = Float.max 0. (height -. bot_height -. top_height) /. Float.of_int slices
  and twist = if Float.abs twist > 0. then Some twist else None in
  let transforms =
    List.init (slices + 1) (fun i -> 0., 0., (Float.of_int i *. s) +. z)
    |> Path3d.to_transforms ?scale ?twist
  in
  sweep ?check_valid ?winding ?fn ?fs ?fa ?mode ?caps ?top ?bot ~transforms shape
