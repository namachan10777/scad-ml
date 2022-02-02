module Make (V : Sigs.Vec) = struct
  open Bezier.Make (V)

  let smooth_bez_fill p1 p2 p3 k = [ p1; V.lerp p2 p1 k; p2; V.lerp p2 p3 k; p3 ]

  let bez_corner ?fn ?(fs = Util.fs) ?dist ~curv p1 p2 p3 =
    let ps =
      match dist with
      | Some d ->
        let prev = V.(normalize (sub p1 p2))
        and next = V.(normalize (sub p3 p2)) in
        V.
          [ add p2 (mul_scalar prev d)
          ; add p2 (mul_scalar prev (curv *. d))
          ; p2
          ; add p1 (mul_scalar next (curv *. d))
          ; add p1 (mul_scalar next d)
          ]
      | None   -> smooth_bez_fill p1 p2 p3 curv
    in
    let fn =
      Int.max 3 (Option.value ~default:Float.(to_int @@ ceil (travel ps /. fs)) fn)
    in
    curve ~fn (bez ps)

  let chamf_corner ~dist p1 p2 p3 =
    let prev = V.(normalize (sub p1 p2))
    and next = V.(normalize (sub p3 p2)) in
    V.[ add p2 (mul_scalar prev dist); add p2 (mul_scalar next dist) ]

  (* let circle_corner ~dist ~rad p1 p2 p3 = *)
  (*   let prev = V.(normalize (sub p1 p2)) *)
  (*   and next = V.(normalize (sub p3 p2)) *)
  (*   and angle = V.(angle_points p1 p2 p3) /. 2. in *)
  (*   if Math.approx angle (Float.pi /. 2.) *)
  (*   then V.[ add p2 (mul_scalar prev dist); add p2 (mul_scalar next dist) ] *)
  (*   else [] *)
end
