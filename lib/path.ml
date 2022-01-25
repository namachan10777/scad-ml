let to_transforms path =
  let p = Array.of_list path in
  let len = Array.length p in
  let f i =
    let open Vec3 in
    let tangent =
      ( if i = 0
      then p.(1) <-> p.(0)
      else if i = len - 1
      then p.(i) <-> p.(i - 1)
      else p.(i + 1) <-> p.(i - 1) )
      |> normalize
    in
    Quaternion.(to_multmatrix ~trans:p.(i) @@ alignment (0., 0., 1.) tangent)
  in
  List.init len f
