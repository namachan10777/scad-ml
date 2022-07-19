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
          List.map2 (fun a b -> Vec3.lerp a b (Float.of_int j *. step)) last next :: acc
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
