(* placeholder module, will likely have this stuff in poly3d as rounded_extrude *)
module R2 = Rounding.Make (Vec2) (Path2d)

type offset =
  { d : float
  ; z : float
  }

type spec = Spec : offset list -> spec

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
  Spec [ { d = -.width; z = Float.abs height } ]

let circ ?(fn = 16) spec =
  let radius = radius_of_spec spec in
  let step = Float.pi /. 2. /. Float.of_int (Int.max 3 fn) in
  let f i =
    let i = Float.of_int (i + 1) in
    { d = quantize (radius *. (Float.cos (i *. step) -. 1.))
    ; z = quantize (Float.abs radius *. Float.sin (i *. step))
    }
  in
  Spec (List.init fn f)

let tear ?(fn = 16) spec =
  let radius = radius_of_spec spec in
  let step = Float.pi /. 4. /. Float.of_int (Int.max 3 fn) in
  let f i =
    if i < fn
    then (
      let i = Float.of_int (i + 1) in
      { d = quantize (radius *. (Float.cos (i *. step) -. 1.))
      ; z = quantize (Float.abs radius *. Float.sin (i *. step))
      } )
    else { d = -2. *. radius *. (1. -. (Float.sqrt 2. /. 2.)); z = Float.abs radius }
  in
  Spec (List.init (fn + 1) f)

let bez ?(curv = 0.5) ?(fn = 16) spec =
  let joint =
    match spec with
    | `Joint j -> j
    | `Cut c   -> 16. *. c /. Float.sqrt 2. /. (1. +. (4. *. curv))
  in
  Spec
    ( R2.bez_corner
        ~fn:(Int.max 1 fn + 2)
        ~curv
        ~spec:(`Joint joint)
        (0., 0.)
        (0., Float.abs joint)
        (-.joint, Float.abs joint)
    |> List.tl
    |> List.map (fun (d, z) -> { d = quantize d; z = quantize z }) )

let custom l =
  Spec
    (List.map (fun { d; z } -> { d = quantize d *. -1.; z = quantize @@ Float.abs z }) l)

let flip_d (Spec l) = Spec (List.map (fun { d; z } -> { d = d *. -1.; z }) l)

let polyhole_partition ?rev ~holes outer =
  let plane = Plane.of_normal ~point:(List.hd outer) @@ Path3d.normal outer in
  let project = List.map @@ Plane.project plane
  and lift = Plane.lift plane in
  let holes = List.map project holes in
  let points, faces = PolyHoles.partition ?rev ~lift ~holes (project outer) in
  Poly3d.make ~points ~faces

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
    let f (pts, faces, start_idx, last_shape, last_len, last_d) { d; z } =
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
    List.hd points, Poly3d.make ~points:List.(concat (rev points)) ~faces
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

(* TODO: think about the API here. Should it be separate functions since there
    are so many optionals, including ones that are only relevant for holes? Or no? *)
let sweep
    ?check_valid
    ?winding
    ?fn
    ?fs
    ?fa
    ?mode
    ?caps
    ?top
    ?bot
    ?holes
    ?(flip_hole_top_d = false)
    ?(flip_hole_bot_d = false)
    ~transforms
    shape
  =
  let sweep = sweep' ?check_valid ?fn ?fs ?fa ?mode ~transforms in
  match holes with
  | None       ->
    let _, _, poly = sweep ?winding ?caps ?top ?bot shape in
    poly
  | Some holes ->
    let tunnel_bots, tunnel_tops, tunnels =
      let hole_bot_spec = if flip_hole_bot_d then Option.map flip_d bot else bot
      and hole_top_spec = if flip_hole_top_d then Option.map flip_d top else top in
      let f (bots, tops, tuns) hole =
        let bot, top, tunnel =
          sweep ~winding:`CW ~caps:`Open ?top:hole_top_spec ?bot:hole_bot_spec hole
        in
        bot :: bots, top :: tops, tunnel :: tuns
      in
      List.fold_left f ([], [], []) holes
    in
    let outer_bot, outer_top, outer = sweep ~winding:`CCW ~caps:`Open ?top ?bot shape in
    let bot_lid = polyhole_partition ~rev:true ~holes:tunnel_bots outer_bot
    and top_lid = polyhole_partition ~holes:tunnel_tops outer_top in
    Poly3d.join (Poly3d.translate (0., 0., 0.) bot_lid :: top_lid :: outer :: tunnels)

(* TODO: add holes *)
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
  let bot_height = List.fold_left (fun _ { z; _ } -> z) 0. bot_spec
  and top_height = List.fold_left (fun _ { z; _ } -> z) 0. top_spec in
  let z = if center then height /. -2. else bot_height
  and s = Float.max 0. (height -. bot_height -. top_height) /. Float.of_int slices
  and twist = if Float.abs twist > 0. then Some twist else None in
  let transforms =
    List.init (slices + 1) (fun i -> 0., 0., (Float.of_int i *. s) +. z)
    |> Path3d.to_transforms ?scale ?twist
  in
  sweep ?check_valid ?winding ?fn ?fs ?fa ?mode ?caps ?top ?bot ~transforms shape

type patch_edges =
  { left : Path3d.t
  ; right : Path3d.t
  ; top : Path3d.t
  ; bot : Path3d.t
  }

let degenerate_patch ?(fn = 16) ?(rev = false) bezpatch =
  let trans_bezpatch = Math.transpose bezpatch
  and n_rows, n_cols = Math.mat_dims bezpatch in
  let row_degen = Array.map (Util.array_all_equal @@ Vec3.approx ~eps:0.) bezpatch
  and col_degen =
    Array.map (Util.array_all_equal @@ Vec3.approx ~eps:0.) trans_bezpatch
  in
  let top_degen = row_degen.(0)
  and bot_degen = row_degen.(n_rows - 1)
  and left_degen = col_degen.(0)
  and right_degen = col_degen.(n_cols - 1)
  and all_rows_degen = Array.for_all Fun.id row_degen
  and all_cols_degen = Array.for_all Fun.id col_degen
  and top_degen_case ~rev bp =
    let row_max =
      let full_degen =
        let row_degen = Array.map (Util.array_all_equal @@ Vec3.approx ~eps:0.) bp in
        let r = Float.(to_int @@ ceil ((of_int n_rows /. 2.) -. 1.)) in
        n_rows >= 4 && Array.for_all Fun.id (Array.sub row_degen 1 r)
      in
      let f = if full_degen then Fun.id else fun i -> if i <= fn / 2 then 2 * i else fn in
      Array.init (fn + 1) f
    and bezpatch =
      Array.map (fun row -> Bezier3d.(curve' @@ make' row)) bp |> Math.transpose
    in
    let pts =
      [ bezpatch.(0).(0) ]
      :: List.init fn (fun i ->
             let fn = row_max.(i) + 2 in
             Bezier3d.(curve ~fn @@ make' bezpatch.(i + 1)) )
    in
    let left = List.map List.hd pts
    and right = List.map Util.last_element pts
    and mesh = Poly3d.tri_mesh ~reverse:(not rev) pts in
    mesh, { left; right; top = List.hd pts; bot = Util.last_element pts }
  in
  match all_rows_degen, all_cols_degen, top_degen, bot_degen, left_degen, right_degen with
  | true, true, _, _, _, _ ->
    let p = [ bezpatch.(0).(0) ] in
    Poly3d.empty, { left = p; right = p; top = p; bot = p }
  | true, false, _, _, _, _ ->
    let col = Bezier3d.(curve ~fn @@ make' trans_bezpatch.(0)) in
    let bot = [ Util.last_element col ] in
    Poly3d.empty, { left = col; right = col; top = [ List.hd col ]; bot }
  | false, true, _, _, _, _ ->
    let row = Bezier3d.(curve ~fn @@ make' bezpatch.(0)) in
    let right = [ Util.last_element row ] in
    Poly3d.empty, { left = [ List.hd row ]; right; top = row; bot = row }
  | false, false, false, false, false, false ->
    let pts = Bezier3d.(patch_curve ~fn @@ patch' bezpatch) in
    let left = List.map List.hd pts
    and right = List.map Util.last_element pts
    and mesh = Poly3d.tri_mesh ~reverse:(not rev) pts in
    mesh, { left; right; top = List.hd pts; bot = Util.last_element pts }
  | false, false, true, true, _, _ ->
    let row_count =
      let steps = ((fn - 3) / 2) + 1 in
      let even = fn mod 2 = 0 in
      let mid_start = if even then steps + 1 else steps in
      let a = Array.make ((steps * 2) + if even then 1 else 0) 0 in
      if even then a.(steps) <- fn + 1;
      for i = 0 to steps - 1 do
        a.(i) <- 3 + (i * 2);
        a.(mid_start + i) <- 3 + ((steps - 1 - i) * 2)
      done;
      a
    in
    let bezpatch =
      Array.map (fun row -> Bezier3d.(curve' @@ make' row)) trans_bezpatch
      |> Math.transpose
    in
    let pts =
      [ bezpatch.(0).(0) ]
      :: Util.fold_init
           (fn - 1)
           (fun j acc ->
             let i = fn - 2 - j in
             Bezier3d.(curve ~fn:row_count.(i) @@ make' bezpatch.(i + 1)) :: acc )
           [ [ bezpatch.(0).(Array.length bezpatch - 1) ] ]
    in
    let left = List.map List.hd pts
    and right = List.map Util.last_element pts
    and mesh = Poly3d.tri_mesh ~reverse:(not rev) pts in
    mesh, { left; right; top = List.hd pts; bot = Util.last_element pts }
  | false, false, true, false, false, false -> top_degen_case ~rev trans_bezpatch
  | false, false, false, true, false, false ->
    let poly, { left; right; top; bot } =
      top_degen_case
        ~rev:(not rev)
        (Array.init n_rows (fun i -> bezpatch.(n_rows - 1 - i)))
    in
    poly, { left; right; top = bot; bot = top }
  | _ ->
    let poly, { left; right; top; bot } =
      top_degen_case ~rev:(not rev) (Math.transpose bezpatch)
    in
    poly, { left = top; right = bot; top = left; bot = right }

(* rtop is either scalar or a tuple in bosl, I'll just enforce tuple input here
    (can deal with converting from config params in rounded_prism) *)
(* rsides is an array of tuples (r/joint value for either side of edge) *)
(* ksides is an array of k values. ktop is scalar (applies to whole top) *)
(* concave is bool for whether the corner idx is concave *)
(* in args to rounded_prism, these could be a flat value, but for simplicity,
   should be expanded out to the correct length (n) before being given to this
   internal function. (and length of the lists given to rounded prism should be
   confirmed to be the correct length when converted to array there as well
   (invalid_arg is not correct)) *)
(* return an array/list of patches of length n (length of top) *)
let compute_patches ~rtop:(rt_in, rt_down) ~rsides ~ktop ~ksides ~concave top bot =
  let len = Array.length top
  and plane = Plane.make top.(0) top.(1) top.(2)
  and rt_in_sign = if rt_in >= 0. then 1. else -1.
  and abs_rt_in = Float.abs rt_in in
  let f i =
    let rside_prev, rside_next = rsides.(i)
    and concave_sign = rt_in_sign *. if concave.(i) then -1. else 1.
    and prev = Vec3.sub top.(Util.index_wrap ~len (i - 1)) top.(i)
    and next = Vec3.sub top.(Util.index_wrap ~len (i + 1)) top.(i)
    and edge = Vec3.sub bot.(i) top.(i) in
    let prev_offset =
      Vec3.(
        div_scalar
          (add top.(i) (mul_scalar (normalize prev) rside_prev))
          (Float.sin Vec3.(angle prev edge)))
    and next_offset =
      Vec3.(
        div_scalar
          (add top.(i) (mul_scalar (normalize next) rside_next))
          (Float.sin Vec3.(angle next edge)))
    and down =
      let edge_angle =
        rt_down /. Float.sin (Float.abs (Plane.line_angle plane (bot.(i), top.(i))))
      in
      Vec3.(mul_scalar (normalize edge) edge_angle)
    and fill_row p1 p2 p3 =
      [| p1; Vec3.lerp p2 p1 ksides.(i); p2; Vec3.lerp p2 p3 ksides.(i); p3 |]
    in
    let row0 =
      let in_prev =
        let a = Vec3.(sub next (mul_scalar prev (dot next prev /. dot prev prev))) in
        Vec3.(mul_scalar (normalize a) concave_sign)
      and in_next =
        let a = Vec3.(sub prev (mul_scalar next (dot prev next /. dot next next))) in
        Vec3.(mul_scalar (normalize a) concave_sign)
      and far_corner =
        let num =
          Vec3.(mul_scalar (normalize (add (normalize prev) (normalize next))) abs_rt_in)
        in
        Vec3.(div_scalar (add num top.(i)) (Float.sin (Vec3.angle prev next /. 2.)))
      in
      let prev_corner = Vec3.(add prev_offset (mul_scalar in_prev abs_rt_in))
      and next_corner = Vec3.(add next_offset (mul_scalar in_next abs_rt_in)) in
      if concave_sign < 0.
      then fill_row prev_corner far_corner next_corner
      else (
        let fc2 = Vec2.of_vec3 far_corner in
        let prev_degen =
          let po2 = Vec2.of_vec3 prev_offset in
          Vec2.(
            line_intersection
              ~bounds1:(true, false)
              ~bounds2:(true, false)
              (fc2, add fc2 (of_vec3 prev))
              (po2, add po2 (of_vec3 in_prev)))
          |> Option.is_none
        and next_degen =
          let no2 = Vec2.of_vec3 next_offset in
          Vec2.(
            line_intersection
              ~bounds1:(true, false)
              ~bounds2:(true, false)
              (fc2, add fc2 (of_vec3 next))
              (no2, add no2 (of_vec3 in_next)))
          |> Option.is_none
        in
        fill_row
          (if prev_degen then far_corner else prev_corner)
          far_corner
          (if next_degen then far_corner else next_corner) )
    and row2 = fill_row prev_offset top.(i) next_offset
    and row4 =
      Vec3.(fill_row (add prev_offset down) (add top.(i) down) (add next_offset down))
    in
    let row1 = Array.map2 (fun a b -> Vec3.lerp a b ktop) row0 row2
    and row3 = Array.map2 (fun a b -> Vec3.lerp a b ktop) row2 row4 in
    [| row0; row1; row2; row3; row4 |]
  in
  Array.init len f
