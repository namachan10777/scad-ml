(* placeholder module, will likely have this stuff in poly3d as rounded_extrude *)
module R = Rounding.Make (Vec2) (Path2d)

(* type chamf = *)
(*   [ `Width of float *)
(*   | `Height of float *)
(*   | `Angle of float *)
(*   ] *)

(* type circ = *)
(*   [ `Radius of float *)
(*   | `Cut of float *)
(*   ] *)

(* type spec = *)
(*   | Chamf of chamf * chamf *)
(*   | Circ of circ *)
(*   | Tear of circ *)
(*   | Bez of *)
(*       { spec : [ `Joint of float | `Cut of float ] *)
(*       ; curv : float *)
(*       } *)
(*   | Custom of (float * float) list *)

(* type cap = *)
(*   { fn : int *)
(*   ; spec : spec *)
(*   } *)

let quantize = Math.quant ~q:(1. /. 1024.)

let radius_of_spec = function
  | `Radius r -> r
  | `Cut c    -> c /. (Float.sqrt 2. -. 1.)

let chamf spec_a spec_b =
  let width, height =
    match spec_a, spec_b with
    | `Width w, `Height h | `Height h, `Width w -> w, h
    | `Cut c, `Angle a | `Angle a, `Cut c -> c /. Float.cos a, c /. Float.sin a
    | `Width w, `Angle a | `Angle a, `Width w -> w, w /. Float.tan a
    | `Height h, `Angle a | `Angle a, `Height h -> h *. Float.tan a, h
    | _ -> raise (Invalid_argument "Same chamfer specification provided twice.")
  in
  [ -.width, height ]

let circ ?(fn = 16) spec =
  let radius = radius_of_spec spec in
  let step = Float.pi /. 2. /. Float.of_int fn in
  let f i =
    let i = Float.of_int (i + 1) in
    ( quantize (radius *. (Float.cos (i *. step) -. 1.))
    , quantize (Float.abs radius *. Float.sin (i *. step)) )
  in
  List.init fn f

let tear ?(fn = 16) spec =
  let radius = radius_of_spec spec in
  let step = Float.pi /. 4. /. Float.of_int fn in
  let f i =
    if i < fn
    then (
      let i = Float.of_int (i + 1) in
      ( quantize (radius *. (Float.cos (i *. step) -. 1.))
      , quantize (Float.abs radius *. Float.sin (i *. step)) ) )
    else -2. *. radius *. (1. -. (Float.sqrt 2. /. 2.)), Float.abs radius
  in
  List.init (fn + 1) f

let bez ?(curv = 0.5) ?(fn = 16) spec =
  let joint =
    match spec with
    | `Joint j -> j
    | `Cut c   -> 16. *. c /. Float.sqrt 2. /. (1. +. (4. *. curv))
  in
  R.bez_corner
    ~fn:(fn + 2)
    ~curv
    ~spec:(`Joint joint)
    (0., 0.)
    (0., Float.abs joint)
    (-.joint, Float.abs joint)
  |> List.tl
  |> List.map (fun (d, z) -> quantize d, quantize z)

let custom l = List.map (fun (d, z) -> quantize d *. -1., quantize z) l

let offset_sweep
    ?check_valid
    ?fn
    ?fs
    ?fa
    ?(mode = `Radius)
    ?(top = [])
    ?(bot = [])
    ~transforms
    shape
  =
  let shape =
    if Float.equal (Path2d.clockwise_sign shape) 1. then List.rev shape else shape
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
      and z = Float.abs z *. z_dir in
      let n, ps, fs = offset ~flip_faces ~start_idx spec last_shape in
      lift ~z m ps :: pts, fs :: faces, start_idx + last_len, ps, n, d
    in
    let points, faces, idx, last_shape, last_len, _ =
      List.fold_left f ([ lift m shape ], [], 0, shape, len, 0.) offsets
    in
    let faces =
      let f =
        if flip_faces then fun i _ -> last_len + idx - i - 1 else fun i _ -> i + idx
      in
      List.mapi f last_shape :: List.concat faces
    in
    Poly3d.make ~points:List.(concat (rev points)) ~faces
  in
  match transforms with
  | []      ->
    let bot = cap ~top:false ~m:MultMatrix.id bot
    and top = cap ~top:true ~m:MultMatrix.id top in
    Poly3d.join [ bot; top ]
  | hd :: _ ->
    let mid, last_transform =
      let f (acc, _last) m = lift m shape :: acc, m in
      List.fold_left f ([], hd) transforms
    in
    let mid = Poly3d.of_layers (List.rev mid)
    and bot = cap ~top:false ~m:hd bot
    and top = cap ~top:true ~m:last_transform top in
    Poly3d.join [ bot; mid; top ]

let extrude
    ?check_valid
    ?fn
    ?fs
    ?fa
    ?slices
    ?scale
    ?(twist = 0.)
    ?(center = false)
    ?mode
    ?(top = [])
    ?(bot = [])
    ~height
    shape
  =
  let slices = Util.helical_slices ?fn:slices twist in
  let bot_height = Float.abs (List.fold_left (fun _ (_, z) -> z) 0. bot)
  and top_height = Float.abs (List.fold_left (fun _ (_, z) -> z) 0. top) in
  let z = if center then height /. -2. else bot_height
  and s = Float.max 0. (height -. bot_height -. top_height) /. Float.of_int slices
  and twist = if Float.abs twist > 0. then Some twist else None in
  let transforms =
    List.init (slices + 1) (fun i -> 0., 0., (Float.of_int i *. s) +. z)
    |> Path3d.to_transforms ?scale ?twist
  in
  offset_sweep ?check_valid ?fn ?fs ?fa ?mode ~top ~bot ~transforms shape
