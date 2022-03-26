open Util
open Vec

(* placeholder module, will likely have this stuff in poly3d as rounded_extrude *)
module R2 = Rounding.Make (Vec2) (Path2)

type offset =
  { d : float
  ; z : float
  }

type spec = Spec : offset list -> spec

type hole_spec =
  [ `Same
  | `Flip
  | `Custom of spec
  ]

type hole =
  { hole : Vec2.t list
  ; top_spec : hole_spec option
  ; bot_spec : hole_spec option
  }

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
        Vec2.zero
        (v2 0. (Float.abs joint))
        (v2 (-.joint) (Float.abs joint))
    |> List.tl
    |> List.map (fun { x = d; y = z } -> { d = quantize d; z = quantize z }) )

let custom l =
  Spec
    (List.map (fun { d; z } -> { d = quantize d *. -1.; z = quantize @@ Float.abs z }) l)

let flip_d (Spec l) = Spec (List.map (fun { d; z } -> { d = d *. -1.; z }) l)

let hole ?(bot = Some `Flip) ?(top = Some `Flip) shape =
  { hole = shape; bot_spec = bot; top_spec = top }

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
      | `CCW     -> Path2.is_clockwise shape
      | `CW      -> not @@ Path2.is_clockwise shape
      | `NoCheck -> false
    in
    if reverse then List.rev shape else shape
  in
  let (Spec top) = Option.value ~default:(Spec []) top
  and (Spec bot) = Option.value ~default:(Spec []) bot
  and len = List.length shape
  and offset = Offset.offset_with_faces ?check_valid ?fn ?fs ?fa
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
    List.hd points, Mesh.make ~points:List.(concat (rev points)) ~faces
  in
  match transforms with
  | []       ->
    let bot_lid, bot = cap ~top:false ~m:MultMatrix.id bot
    and top_lid, top = cap ~top:true ~m:MultMatrix.id top in
    bot_lid, top_lid, Mesh.join [ bot; top ]
  | hd :: tl ->
    let mid, last_transform =
      let f (acc, _last) m = lift m shape :: acc, m in
      List.fold_left f (f ([], hd) hd) tl
    in
    let mid = Mesh.of_layers ~caps:`Open (List.rev mid)
    and bot_lid, bot = cap ~top:false ~m:hd bot
    and top_lid, top = cap ~top:true ~m:last_transform top in
    bot_lid, top_lid, Mesh.join [ bot; mid; top ]

(* TODO: think about the API here. Should it be separate functions since there
    are so many optionals, including ones that are only relevant for holes? Or no? *)
let sweep ?check_valid ?winding ?fn ?fs ?fa ?mode ?caps ?top ?bot ?holes ~transforms shape
  =
  let sweep = sweep' ?check_valid ?fn ?fs ?fa ?mode ~transforms in
  match holes with
  | None       ->
    let _, _, poly = sweep ?winding ?caps ?top ?bot shape in
    poly
  | Some holes ->
    let tunnel_bots, tunnel_tops, tunnels =
      let hole_spec outer_spec = function
        | Some `Same          -> outer_spec
        | Some `Flip          -> Option.map flip_d outer_spec
        | Some (`Custom spec) -> Some spec
        | None                -> None
      in
      let f (bots, tops, tuns) { hole; bot_spec; top_spec } =
        let bot = hole_spec bot bot_spec
        and top = hole_spec top top_spec in
        let bot, top, tunnel = sweep ~winding:`CW ~caps:`Open ?top ?bot hole in
        bot :: bots, top :: tops, tunnel :: tuns
      in
      List.fold_left f ([], [], []) holes
    in
    let outer_bot, outer_top, outer = sweep ~winding:`CCW ~caps:`Open ?top ?bot shape in
    let bot_lid = Mesh.of_poly3 ~rev:true (Poly3.make ~holes:tunnel_bots outer_bot)
    and top_lid = Mesh.of_poly3 (Poly3.make ~holes:tunnel_tops outer_top) in
    Mesh.join (bot_lid :: top_lid :: outer :: tunnels)

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
    ?holes
    ~height
    shape
  =
  let slices = helical_slices ?fa ?fn:slices twist
  and (Spec top_spec) = Option.value ~default:(Spec []) top
  and (Spec bot_spec) = Option.value ~default:(Spec []) bot in
  let bot_height = List.fold_left (fun _ { z; _ } -> z) 0. bot_spec
  and top_height = List.fold_left (fun _ { z; _ } -> z) 0. top_spec in
  let z = if center then height /. -2. else bot_height
  and s = Float.max 0. (height -. bot_height -. top_height) /. Float.of_int slices
  and twist = if Float.abs twist > 0. then Some twist else None in
  let transforms =
    List.init (slices + 1) (fun i -> v3 0. 0. ((Float.of_int i *. s) +. z))
    |> Path3.to_transforms ?scale ?twist
  in
  sweep ?check_valid ?winding ?fn ?fs ?fa ?mode ?caps ?top ?bot ?holes ~transforms shape

type patch_edges =
  { left : Path3.t
  ; right : Path3.t
  ; top : Path3.t
  ; bot : Path3.t
  }

let degenerate_patch ?(fn = 16) ?(rev = false) bezpatch =
  let trans_bezpatch = Math.transpose bezpatch
  and n_rows, n_cols = Math.mat_dims bezpatch in
  let row_degen = Array.map (array_all_equal @@ Vec3.approx ~eps:0.) bezpatch
  and col_degen = Array.map (array_all_equal @@ Vec3.approx ~eps:0.) trans_bezpatch in
  let top_degen = row_degen.(0)
  and bot_degen = row_degen.(n_rows - 1)
  and left_degen = col_degen.(0)
  and right_degen = col_degen.(n_cols - 1)
  and all_rows_degen = Array.for_all Fun.id row_degen
  and all_cols_degen = Array.for_all Fun.id col_degen
  and top_degen_case ~rev bp =
    let row_max =
      let full_degen =
        let row_degen = Array.map (array_all_equal @@ Vec3.approx ~eps:0.) bp in
        let r = Float.(to_int @@ ceil ((of_int n_rows /. 2.) -. 1.)) in
        n_rows >= 4 && Array.for_all Fun.id (Array.sub row_degen 1 r)
      in
      let f = if full_degen then Fun.id else fun i -> if i <= fn / 2 then 2 * i else fn in
      Array.init (fn + 1) f
    and bezpatch =
      Array.map (fun row -> Bezier3.(curve' @@ make' row)) bp |> Math.transpose
    in
    let pts =
      [ bezpatch.(0).(0) ]
      :: List.init fn (fun i ->
             let fn = row_max.(i) + 2 in
             Bezier3.(curve ~fn @@ make' bezpatch.(i + 1)) )
    in
    let left = List.map List.hd pts
    and right = List.map last_element pts
    and mesh = Mesh.of_ragged ~rev:(not rev) pts in
    mesh, { left; right; top = List.hd pts; bot = last_element pts }
  in
  match all_rows_degen, all_cols_degen, top_degen, bot_degen, left_degen, right_degen with
  | true, true, _, _, _, _ ->
    let p = [ bezpatch.(0).(0) ] in
    Mesh.empty, { left = p; right = p; top = p; bot = p }
  | true, false, _, _, _, _ ->
    let col = Bezier3.(curve ~fn @@ make' trans_bezpatch.(0)) in
    let bot = [ last_element col ] in
    Mesh.empty, { left = col; right = col; top = [ List.hd col ]; bot }
  | false, true, _, _, _, _ ->
    let row = Bezier3.(curve ~fn @@ make' bezpatch.(0)) in
    let right = [ last_element row ] in
    Mesh.empty, { left = [ List.hd row ]; right; top = row; bot = row }
  | false, false, false, false, false, false ->
    let pts = Bezier3.(patch_curve ~fn @@ patch' bezpatch) in
    let left = List.map List.hd pts
    and right = List.map last_element pts
    and mesh = Mesh.of_ragged ~rev:(not rev) pts in
    mesh, { left; right; top = List.hd pts; bot = last_element pts }
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
      Array.map (fun row -> Bezier3.(curve' @@ make' row)) trans_bezpatch
      |> Math.transpose
    in
    let pts =
      [ bezpatch.(0).(0) ]
      :: fold_init
           (fn - 1)
           (fun j acc ->
             let i = fn - 2 - j in
             Bezier3.(curve ~fn:row_count.(i) @@ make' bezpatch.(i + 1)) :: acc )
           [ [ bezpatch.(0).(Array.length bezpatch - 1) ] ]
    in
    let left = List.map List.hd pts
    and right = List.map last_element pts
    and mesh = Mesh.of_ragged ~rev:(not rev) pts in
    mesh, { left; right; top = List.hd pts; bot = last_element pts }
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

let compute_patches ~r_top:(rt_in, rt_down) ~r_sides ~k_top ~k_sides ~concave top bot =
  let len = Array.length top
  and plane = Plane.make top.(0) top.(1) top.(2)
  and rt_in_sign = if rt_in >= 0. then 1. else -1.
  and abs_rt_in = Float.abs rt_in in
  let f i =
    let rside_prev, rside_next = r_sides.(i)
    and concave_sign = rt_in_sign *. if concave.(i) then -1. else 1.
    and prev = Vec3.sub top.(index_wrap ~len (i - 1)) top.(i)
    and next = Vec3.sub top.(index_wrap ~len (i + 1)) top.(i)
    and edge = Vec3.sub bot.(i) top.(i) in
    let prev_offset =
      let s = Vec3.(smul (normalize prev) (rside_prev /. Float.sin (angle prev edge))) in
      Vec3.add top.(i) s
    and next_offset =
      let s = Vec3.(smul (normalize next) (rside_next /. Float.sin (angle next edge))) in
      Vec3.add top.(i) s
    and down =
      let edge_angle =
        rt_down /. Float.sin (Float.abs (Plane.line_angle plane (bot.(i), top.(i))))
      in
      Vec3.(smul (normalize edge) edge_angle)
    and fill_row p1 p2 p3 =
      [| p1; Vec3.lerp p2 p1 k_sides.(i); p2; Vec3.lerp p2 p3 k_sides.(i); p3 |]
    in
    let row0 =
      let in_prev =
        let a = Vec3.(sub next (smul prev (dot next prev /. dot prev prev))) in
        Vec3.(smul (normalize a) concave_sign)
      and in_next =
        let a = Vec3.(sub prev (smul next (dot prev next /. dot next next))) in
        Vec3.(smul (normalize a) concave_sign)
      and far_corner =
        let num =
          let s = concave_sign *. abs_rt_in in
          Vec3.(smul (normalize (add (normalize prev) (normalize next))) s)
        in
        Vec3.(add top.(i) @@ sdiv num (Float.sin (Vec3.angle prev next /. 2.)))
      in
      let prev_corner = Vec3.(add prev_offset (smul in_prev abs_rt_in))
      and next_corner = Vec3.(add next_offset (smul in_next abs_rt_in)) in
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
              { a = fc2; b = add fc2 (of_vec3 prev) }
              { a = po2; b = add po2 (of_vec3 in_prev) })
          |> Option.is_none
        and next_degen =
          let no2 = Vec2.of_vec3 next_offset in
          Vec2.(
            line_intersection
              ~bounds1:(true, false)
              ~bounds2:(true, false)
              { a = fc2; b = add fc2 (of_vec3 next) }
              { a = no2; b = add no2 (of_vec3 in_next) })
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
    let row1 = Array.map2 (fun a b -> Vec3.lerp b a k_top) row0 row2
    and row3 = Array.map2 (fun a b -> Vec3.lerp a b k_top) row2 row4 in
    [| row0; row1; row2; row3; row4 |]
  in
  Array.init len f

let curvature_continuity ~len ~bot_patch:bp ~top_patch:tp =
  let check line =
    if not (Path3.is_collinear line) then failwith "Curvature continuity failure."
  and w = index_wrap ~len in
  let horiz p i j =
    [ p.(i).(j).(2)
    ; p.(i).(j).(3)
    ; p.(i).(j).(4)
    ; p.(w (i + 1)).(j).(0)
    ; p.(w (i + 1)).(j).(1)
    ; p.(w (i + 1)).(j).(2)
    ]
  in
  ignore w;
  for i = 0 to len - 1 do
    for j = 0 to 4 do
      (* verify vertical edges *)
      check
        [ tp.(i).(2).(j)
        ; tp.(i).(3).(j)
        ; tp.(i).(4).(j)
        ; bp.(i).(2).(j)
        ; bp.(i).(3).(j)
        ; bp.(i).(4).(j)
        ];
      (* verify horizontal edges *)
      check (horiz tp i j);
      check (horiz bp i j)
    done
  done

let bad_patches ~len ~bot_patch:bp ~top_patch:tp bot top =
  let open Vec3 in
  let w = index_wrap ~len in
  let vert_bad i acc =
    if distance top.(i) tp.(i).(4).(2) +. distance bot.(i) bp.(i).(4).(2)
       > distance bot.(i) top.(i)
    then i :: acc
    else acc
  and patch_bad p i acc =
    if distance p.(i).(2).(4) p.(i).(2).(2)
       +. distance p.(w (i + 1)).(2).(0) p.(w (i + 1)).(2).(2)
       > distance p.(i).(2).(2) p.(w (i + 1)).(2).(2)
    then (i, (i + 1) mod len) :: acc
    else acc
  and patch_in_bad p i acc =
    if distance p.(i).(0).(2) p.(i).(0).(4)
       +. distance p.(w (i + 1)).(0).(0) p.(w (i + 1)).(0).(2)
       > distance p.(i).(0).(2) p.(w (i + 1)).(0).(2)
    then (i, (i + 1) mod len) :: acc
    else acc
  and show (a, b) = Printf.sprintf "(%i, %i)" a b in
  let check ~show ~msg f =
    match fold_init len f [] with
    | []  -> ()
    | bad ->
      let f acc a = Printf.sprintf "%s; %s" acc (show a) in
      failwith @@ List.fold_left f (Printf.sprintf "%s: [" msg) bad ^ "]"
  in
  check
    ~show:Int.to_string
    ~msg:"Top and bottom joint lengths are too large; they interfere with eachother"
    vert_bad;
  check ~show ~msg:"Joint lengths too large at top edges" (patch_bad tp);
  check ~show ~msg:"Joint lengths too large at bottom edges" (patch_bad bp);
  check ~show ~msg:"Joint length too large on the top face at edges" (patch_in_bad tp);
  check ~show ~msg:"Joint length too large on the bottom face at edges" (patch_in_bad bp)

let roundover_interference label face =
  let proj = Path3.(project (to_plane face) face) in
  if not (Path2.is_simple proj)
  then (
    let msg =
      Printf.sprintf
        "Roundovers interfere with eachother on the %s face: either the shape is self \
         intersecting or the %s joint length is too large."
        label
        label
    in
    failwith msg )

let prism
    ?(debug = false)
    ?(fn = 16)
    ?(k = 0.5)
    ?k_bot
    ?k_top
    ?k_sides
    ?(joint_bot = 0., 0.)
    ?(joint_top = 0., 0.)
    ?(joint_sides = `Flat (0., 0.))
    bottom
    top
  =
  let bottom = Array.of_list bottom
  and top = Array.of_list top in
  let len = Array.length bottom in
  let wrap = index_wrap ~len
  and unpack_sides ~name = function
    | `Flat s -> Array.make len s
    | `Mix ss ->
      let ss = Array.of_list ss in
      if Array.length ss = len
      then ss
      else
        invalid_arg
        @@ Printf.sprintf "`Mix %s must be the same length as the top/bottom polys." name
  in
  if len <> Array.length top
  then invalid_arg "Top and bottom shapes must have the same length.";
  let k_bot = Option.value ~default:k k_bot
  and k_top = Option.value ~default:k k_top
  and k_sides = unpack_sides ~name:"k_sides" (Option.value ~default:(`Flat k) k_sides)
  and r_sides = unpack_sides ~name:"joint_sides" joint_sides in
  let bot_proj =
    let plane = Plane.make bottom.(0) bottom.(1) bottom.(2) in
    Array.map (Plane.project plane) bottom
  in
  let bottom_sign = Path2.clockwise_sign' bot_proj in
  let concave =
    let f i =
      let line = Vec2.{ a = bot_proj.(wrap (i - 1)); b = bot_proj.(i) } in
      bottom_sign *. Vec2.left_of_line ~line bot_proj.(wrap (i + 1)) > 0.
    in
    Array.init len f
  in
  let top_patch =
    compute_patches ~r_top:joint_top ~r_sides ~k_top ~k_sides ~concave top bottom
  and bot_patch =
    compute_patches ~r_top:joint_bot ~r_sides ~k_top:k_bot ~k_sides ~concave bottom top
  in
  if not debug then bad_patches ~len ~bot_patch ~top_patch bottom top;
  let top_samples, top_edges =
    unzip_array @@ Array.map (degenerate_patch ~fn ~rev:false) top_patch
  and bot_samples, bot_edges =
    unzip_array @@ Array.map (degenerate_patch ~fn ~rev:true) bot_patch
  in
  let top_faces = fold_init len (fun i acc -> List.rev_append top_edges.(i).top acc) []
  and bot_faces = fold_init len (fun i acc -> List.rev_append bot_edges.(i).top acc) [] in
  let edge_points =
    let f i acc =
      let top_edge = [ top_edges.(i).right; top_edges.(wrap (i + 1)).left ]
      and bot_edge = [ bot_edges.(wrap (i + 1)).left; bot_edges.(i).right ]
      and vert_edge = [ bot_edges.(i).bot; top_edges.(i).bot ] in
      vert_edge :: bot_edge :: top_edge :: acc
    in
    fold_init len f []
  and faces =
    let patches =
      List.init len (fun i ->
          [ bot_patch.(i).(4).(4)
          ; bot_patch.(wrap (i + 1)).(4).(0)
          ; top_patch.(wrap (i + 1)).(4).(0)
          ; top_patch.(i).(4).(4)
          ] )
    in
    List.rev top_faces :: bot_faces :: patches
  in
  if not debug then curvature_continuity ~len ~bot_patch ~top_patch;
  if not debug then roundover_interference "top" top_faces;
  if not debug then roundover_interference "bottom" bot_faces;
  List.fold_left
    (fun acc pts -> Mesh.of_ragged pts :: acc)
    [ Mesh.of_polygons faces ]
    edge_points
  |> fold_init len (fun i acc -> bot_samples.(i) :: acc)
  |> fold_init len (fun i acc -> top_samples.(i) :: acc)
  |> Mesh.join
