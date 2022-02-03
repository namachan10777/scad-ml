module Make (V : Sigs.Vec) (P : Sigs.ArcProvider with type vec := V.t) = struct
  module B = Bezier.Make (V)

  (* NOTE: seems like it is backwards in bosl, be sure to test. *)
  let smooth_bez_fill ~curv p1 p2 p3 =
    (* [ p1; V.lerp p2 p1 curv; p2; V.lerp p2 p3 curv; p3 ] *)
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
      Int.max 3 (Option.value ~default:Float.(to_int @@ ceil (B.travel ps /. fs)) fn)
    in
    B.curve ~fn (B.bez ps)

  let chamf_corner ~spec p1 p2 p3 =
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
    let dist, rad, cut =
      match spec with
      | `Joint d  ->
        let rad = d *. Float.tan half_angle in
        d, rad, rad /. (Float.sin half_angle -. 1.)
      | `Radius r ->
        r /. Float.tan half_angle, r, r *. ((1. /. Float.sin half_angle) -. 1.)
      | `Cut c    ->
        let rad = c /. ((1. /. Float.sin half_angle) -. 1.) in
        (if is_180 then Float.infinity else rad /. Float.tan half_angle), rad, c
    in
    let p1' = V.(add p2 (mul_scalar (normalize (sub p1 p2)) dist))
    and p3' = V.(add p2 (mul_scalar (normalize (sub p3 p2)) dist)) in
    if is_180
    then [ p1'; p3' ]
    else (
      let cut_vector = V.(normalize @@ sub (mean [ p1'; p3' ]) p2)
      and fn =
        let frags = Float.of_int @@ Util.helical_fragments ?fn ~fa ~fs rad in
        Float.(to_int @@ max 3. @@ ceil (((pi /. 2.) -. half_angle) /. pi *. frags))
      in
      P.arc_through ~fn p1' V.(add p2 (mul_scalar cut_vector cut)) p3' )

  module Specs = struct
    type radius = [ `Radius of float ]
    type joint = [ `Joint of float ]
    type cut = [ `Cut of float ]
    type width = [ `Width of float ]

    type t =
      | Chamfer of { spec : [ joint | cut | width ] }
      | Circle of { spec : [ radius | joint | cut ] }
      | Smooth of
          { spec : [ joint | cut ]
          ; curv : float option
          }

    let to_corner ?fn ?fa ?fs t =
      match t with
      | Chamfer { spec }      -> chamf_corner ~spec
      | Circle { spec }       -> circle_corner ?fn ?fa ?fs ~spec
      | Smooth { spec; curv } -> bez_corner ?fn ?fs ?curv ~spec
  end

  let valid_specs ~closed ~specs shape =
    let len_shape = Array.length shape
    and len_specs = Array.length specs in
    if closed && len_shape <> len_specs
    then raise (Invalid_argument "When closed, lengths of shape and spec must be equal.");
    if (not closed)
       && ( (not
               ( len_shape = len_specs
               && Option.(is_none specs.(0) && is_none specs.(len_shape - 1)) ) )
          || Array.length specs <> len_shape - 2 )
    then
      raise
        (Invalid_argument
           "When not closed (path), then the first and last points of specs must be \
            None, or its length must be 2 shorter." )

  let round_corners ?(closed = true) ?fn ?fa ?fs ~specs shape =
    let shape = Array.of_list shape in
    let len = Array.length shape in
    let specs =
      match specs with
      | `Flat spec  ->
        let f i = if (not closed) && (i = 0 || i = len - 1) then None else Some spec in
        Array.init len f
      | `List specs ->
        let specs = Array.of_list specs in
        valid_specs ~closed ~specs shape;
        specs
    in
    let wrap = Util.index_wrap ~len in
    let f i =
      match specs.(i) with
      | Some spec ->
        let corner = Specs.to_corner ?fn ?fa ?fs spec in
        corner shape.(wrap (i - 1)) shape.(i) shape.(wrap (i + 1))
      | None      -> [ shape.(i) ]
    in
    List.init len f |> List.concat |> Util.deduplicate ~equal:V.approx
end
