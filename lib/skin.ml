(* TODO:
   - come up with a design that will allow composing morphing sweep functions
    with relative ease (For those, will need to morph the correct amount between
    steps along the path. If going from path, rather than from transforms, then
    using relative distance along the path would enable a linear morph through
    the sweep)
   - morphing sweep/extrusions will be starting from Poly2 as the others do, and
    support rounded caps
   - I think a ~k curvature hardness parameter would be
    nice to have, so that morphing doesn't have to be strictly linear between
    the profiles (bezier with middle control point set between 0 and 1)
   - I think I probably can't get around exposing a "low level" version that
    deals in lists of profiles and parameters, but the simpler bases should all
    be covered by the functions that only take two profiles as parameters.
   - read through and understand how I should implement the types such that
    efficient upsampling timing like in BOSL2 can be achieved without too much
    headache or dynamism
   - need to watch out for the impact that duplicated vertices will have on
    integration with sweeping / polyholes since I have checks in some places
    that would catch it as an incorrect polygon.
    *)

module Bez = Bezier.Make (Vec3)

type spec =
  [ `Direct
  | `Reindex
  | `Distance
  | `FastDistance
  | `Tangent
  ]

type slices =
  [ `Flat of int
  | `Mix of int list
  ]

let bezier_transition ?(k = 0.5) ~fn ~init a b =
  let step = 1. /. Float.of_int fn
  and bezs = List.map2 (fun a b -> Bez.make [ a; Vec3.lerp a b k; b ]) a b in
  let f j acc =
    let u = Float.of_int j *. step in
    List.map (fun bez -> bez u) bezs :: acc
  in
  Util.fold_init fn f init

let slice_profiles ?(closed = false) ~slices = function
  | [] | [ _ ]        -> invalid_arg "Too few profiles to slice."
  | hd :: tl as profs ->
    let len = List.length profs in
    let get_slices =
      match slices with
      | `Flat n -> Fun.const n
      | `Mix l  ->
        let a = Array.of_list l
        and n = len - if closed then 0 else 1 in
        if Array.length a <> n
        then
          invalid_arg
          @@ Printf.sprintf
               "`Mix slice entries (%i) do not match the number of transitions (%i)"
               (Array.length a)
               n;
        Array.get a
    in
    let f (acc, last, i) next =
      let n = get_slices i in
      let step = 1. /. Float.of_int n in
      let acc =
        let g j acc =
          let u = Float.of_int j *. step in
          List.map2 (fun a b -> Vec3.lerp a b u) last next :: acc
        in
        Util.fold_init n g acc
      in
      acc, next, i + 1
    in
    let profiles, last, i = List.fold_left f ([], hd, 0) tl in
    if closed
    then (
      let profiles, _, _ = f (profiles, last, i) hd in
      List.rev profiles )
    else List.rev profiles
