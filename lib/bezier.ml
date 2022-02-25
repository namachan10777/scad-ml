(* Based on: https://github.com/revarbat/BOSL2/blob/master/beziers.scad *)

module Tbl = Hashtbl.Make (struct
  type t = int

  let equal = Int.equal
  let hash = Fun.id
end)

let signed_pascals_triangle =
  let tbl = Tbl.create 10 in
  Tbl.add tbl 0 (Array.make 1 (-1.));
  fun n ->
    let len = Tbl.length tbl in
    if n > len - 1
    then
      for i = len - 1 to n do
        let row = Array.make (i + 2) (-1.)
        and last = Tbl.find tbl i in
        for j = 0 to i - 1 do
          let sign = if j mod 2 = 1 then -1. else 1. in
          row.(j + 1) <- sign *. (Float.abs last.(j) +. Float.abs last.(j + 1))
        done;
        row.(i + 1) <- (if i mod 2 = 0 then 1. else -1.);
        Tbl.add tbl (i + 1) row
      done
    else ();
    tbl

let bezier_matrix =
  let tbl = Tbl.create 10 in
  fun n ->
    match Tbl.find_opt tbl n with
    | Some m -> m
    | None   ->
      let tri = signed_pascals_triangle n
      and m = Array.make_matrix (n + 1) (n + 1) 0. in
      let nth_row = Tbl.find tri n in
      for i = 0 to n do
        let a = Tbl.find tri i
        and b = nth_row.(i) in
        for j = 0 to i do
          m.(i).(j) <- a.(j) *. b
        done
      done;
      Tbl.add tbl n m;
      m

module type S = sig
  type vec

  val coefs' : vec array -> vec array
  val coefs : vec list -> vec array
  val make' : vec array -> float -> vec
  val make : vec list -> float -> vec

  val curve
    :  ?init:vec list
    -> ?rev:bool
    -> ?fn:int
    -> ?endpoint:bool
    -> (float -> vec)
    -> vec list

  val curve' : ?rev:bool -> ?fn:int -> ?endpoint:bool -> (float -> vec) -> vec array
  val travel : ?start_u:float -> ?end_u:float -> ?max_deflect:float -> vec list -> float
  val patch : vec list list -> float -> float -> vec
  val patch' : vec array array -> float -> float -> vec
  val patch_curve' : ?fn:int -> (float -> float -> vec) -> vec array array
  val patch_curve : ?fn:int -> (float -> float -> vec) -> vec list list
  val of_bezpath : ?n:int -> vec list -> float -> vec

  val bezpath_of_path
    :  ?closed:bool
    -> ?uniform:bool
    -> ?size:
         [> `Abs of float list
         | `FlatAbs of float
         | `FlatRel of float
         | `Rel of float list
         ]
    -> ?tangents:vec list
    -> vec list
    -> vec list

  val bezpath_curve : ?fn:int -> ?n:int -> vec list -> vec list

  val of_path
    :  ?closed:bool
    -> ?uniform:bool
    -> ?size:
         [> `Abs of float list
         | `FlatAbs of float
         | `FlatRel of float
         | `Rel of float list
         ]
    -> ?tangents:vec list
    -> vec list
    -> float
    -> vec

  val closest_point : ?n:int -> ?max_err:float -> (float -> vec) -> vec -> float
end

module Make (V : Vec.S) : S with type vec := V.t = struct
  module P = Path.Make (V)

  let coefs' ps =
    let n = Array.length ps - 1 in
    let bm = bezier_matrix n
    and m = Array.make (n + 1) V.zero in
    for i = 0 to n do
      let row = bm.(i) in
      for j = 0 to n do
        m.(i) <- V.add m.(i) (V.smul ps.(j) row.(j))
      done
    done;
    m

  let coefs ps = coefs' (Array.of_list ps)

  let make' ps =
    let n = Array.length ps - 1 in
    let m = coefs' ps in
    fun u ->
      let pt = ref V.zero in
      for i = 0 to n do
        pt := V.add !pt (V.smul m.(i) (Float.pow u (Float.of_int i)))
      done;
      !pt

  let make ps = make' (Array.of_list ps)

  let curve ?(init = []) ?(rev = false) ?(fn = 16) ?(endpoint = true) bez =
    let step = 1. /. Float.of_int fn in
    let last = if endpoint then 1. else 1. -. step in
    let dt = 1. /. Float.of_int fn *. if rev then last else -.last in
    let rec loop acc i t =
      if i <= fn then loop (bez t :: acc) (i + 1) (t +. dt) else acc
    in
    loop init 0 (if rev then 0. else 1.)

  let curve' ?(rev = false) ?(fn = 16) ?(endpoint = true) bez =
    let a = Array.make (fn + 1) V.zero
    and step = 1. /. Float.of_int fn in
    let last = if endpoint then 1. else 1. -. step in
    let dt = 1. /. Float.of_int fn *. if rev then last else -.last
    and t = ref (if rev then 1. else 0.) in
    for i = 0 to fn do
      a.(i) <- bez !t;
      t := !t +. dt
    done;
    a

  let travel ?(start_u = 0.) ?(end_u = 1.) ?(max_deflect = 0.01) ps =
    let n_segs = List.length ps * 2
    and bz = make ps in
    let d = Float.of_int n_segs in
    let rec aux su eu =
      let path =
        let f i = bz (Math.lerp su eu (Float.of_int i /. d)) in
        Array.init (n_segs + 1) f
      in
      (* maximum deviation from a straight line *)
      let deflection =
        let mx = ref Float.min_float
        and p = Array.unsafe_get path in
        for i = 0 to n_segs - 2 do
          let mid_point = V.sdiv (V.add (p i) (p (i + 2))) 2. in
          mx := Float.max !mx (V.distance (p (i + 1)) mid_point)
        done;
        !mx
      in
      if deflection <= max_deflect
      then P.total_travel' path
      else (
        let sum = ref 0. in
        for i = 0 to n_segs - 1 do
          let i = Float.of_int i in
          let su' = Math.lerp su eu (i /. d)
          and eu' = Math.lerp su eu ((i +. 1.) /. d) in
          sum := !sum +. aux su' eu'
        done;
        !sum )
    in
    aux start_u end_u

  let patch grid =
    let horizontal_bezs = List.map make grid in
    fun u ->
      let vertical_bez = make @@ List.map (fun bz -> bz u) horizontal_bezs in
      vertical_bez

  let patch' grid =
    let horizontal_bezs = Array.map make' grid in
    fun u ->
      let vertical_bez = make' @@ Array.map (fun bz -> bz u) horizontal_bezs in
      vertical_bez

  let patch_curve' ?(fn = 16) p =
    let m = Array.make_matrix (fn + 1) (fn + 1) V.zero
    and step = 1. /. Float.of_int fn in
    for col = 0 to fn do
      let vbez = p (Float.of_int col *. step) in
      for row = 0 to fn do
        m.(row).(col) <- vbez (Float.of_int row *. step)
      done
    done;
    m

  let patch_curve ?(fn = 16) p =
    let m = patch_curve' ~fn p in
    List.init (fn + 1) (fun i -> List.init (fn + 1) (fun j -> m.(i).(j)))

  let of_bezpath ?(n = 3) bezpath =
    let bezpath = Array.of_list bezpath in
    let len = Array.length bezpath in
    if len mod n <> 1
    then (
      let msg =
        Printf.sprintf
          "The length of a degree %i bezier path should be a multiple of %i, plus 1."
          len
          len
      in
      invalid_arg msg );
    let n_segs = (len - 1) / n in
    let segs =
      Array.init n_segs (fun i -> List.init (n + 1) (fun j -> bezpath.((i * n) + j)))
    in
    let bezs = Array.map (fun ps -> make ps) segs in
    let travels, total =
      let f (acc, total) seg =
        let total = total +. travel seg in
        total :: acc, total
      in
      let acc, total = Array.fold_left f ([ 0. ], 0.) segs in
      Util.array_of_list_rev acc, total
    in
    let extrapolate s =
      if s >= 1.
      then bezs.(n_segs - 1) 1.0
      else (
        let d = Float.(min (max s 0.) 1.) *. total in
        let i = ref 0
        and p = ref None in
        while Option.is_none !p && !i < n_segs do
          let idx = !i in
          let d0 = Array.unsafe_get travels idx
          and d1 = Array.unsafe_get travels (idx + 1) in
          if d >= d0 && d <= d1
          then (
            let frac = (d -. d0) /. (d1 -. d0) in
            p := Some (bezs.(idx) frac) )
          else incr i
        done;
        Option.get !p )
    in
    extrapolate

  let bezpath_of_path
      ?(closed = false)
      ?(uniform = false)
      ?(size = `FlatRel 0.1)
      ?tangents
      path
    =
    let ps = Array.of_list path
    and get_size =
      let valid l = List.fold_left (fun m a -> Float.min m a) Float.max_float l > 0. in
      match size with
      | `FlatRel s when s > 0. -> fun _ seg_len -> seg_len *. s
      | `FlatAbs s when s > 0. -> fun _ _ -> s
      | `Rel ss when valid ss ->
        let ss = Array.of_list ss in
        fun i seg_len -> seg_len *. Array.unsafe_get ss i
      | `Abs ss when valid ss ->
        let ss = Array.of_list ss in
        fun i _ -> Array.unsafe_get ss i
      | _ -> invalid_arg "Size must be greater than zero."
    and power_poly =
      let m =
        [| [| -3.; 6.; -3. |]; [| 7.; -9.; 2. |]; [| -5.; 3.; 0. |]; [| 1.; 0.; 0. |] |]
      in
      fun n1 n2 -> Math.matmul m V.[| [| dot n1 n1 |]; [| dot n1 n2 |]; [| dot n2 n2 |] |]
    in
    let tangents =
      ( match tangents with
      | Some tangents -> List.map V.normalize tangents
      | None          -> P.tangents ~uniform ~closed path )
      |> Array.of_list
    in
    let len_ps = Array.length ps
    and len_ts = Array.length tangents in
    let f i acc =
      let p1 = ps.(i)
      and p2 = ps.(Util.index_wrap ~len:len_ps (i + 1)) in
      let seg_len = V.distance p2 p1 in
      let seg_vec = V.(sdiv (sub p2 p1) seg_len)
      and t1 = tangents.(i) (* second tangent pointing backwards *)
      and t2 = V.negate tangents.(Util.index_wrap ~len:len_ts (i + 1)) in
      (* total component of tangents parallel to the segment *)
      let parallel = Float.abs (V.dot t1 seg_vec) +. Float.abs (V.dot t2 seg_vec)
      and normal1 = V.(sub t1 (smul seg_vec (dot t1 seg_vec)))
      and normal2 = V.(sub t2 (smul seg_vec (dot t2 seg_vec))) in
      let p = Array.map (fun a -> a.(0)) (power_poly normal1 normal2) in
      let uextreme =
        let p_norm = Float.sqrt (Array.fold_left (fun sum a -> (a *. a) +. sum) 0. p) in
        if Math.approx p_norm 0.
        then [||]
        else (
          let f acc r = if r > 0. && r < 1. then r :: acc else acc in
          Util.array_of_list_rev (Array.fold_left f [] (Math.real_roots' p)) )
      in
      let dists =
        let bz = make' [| V.zero; normal1; normal2; V.zero |] in
        Array.map (fun u -> V.norm (bz u)) uextreme
      in
      let scale =
        match Array.length dists with
        | 0 -> 0.
        | 1 -> dists.(0)
        | _ ->
          let sum = Array.fold_left ( +. ) 0. dists
          and min = Array.fold_left Float.min 0. dists in
          sum -. (2. *. min)
      in
      let l = Float.min (seg_len /. parallel) (get_size i seg_len /. scale) in
      V.(add p2 (smul t2 l)) :: V.(add p1 (smul t1 l)) :: p1 :: acc
    in
    List.rev (ps.(len_ps - 1) :: Util.fold_init len_ts f [])

  let bezpath_curve ?(fn = 16) ?(n = 3) bezpath =
    let bezpath = Array.of_list bezpath in
    let len = Array.length bezpath in
    if len mod n <> 1
    then (
      let msg =
        Printf.sprintf
          "The length of a degree %i bezier path should be a multiple of %i, plus 1."
          len
          len
      in
      invalid_arg msg );
    let n_segs = (len - 1) / n in
    let f i pts =
      let bez = make' @@ Array.init (n + 1) (fun j -> bezpath.((i * n) + j)) in
      let pts = curve ~init:pts ~rev:true ~fn bez in
      (* avoid duplication of endpoints *)
      if i < n_segs - 1 then List.tl pts else pts
    in
    List.rev @@ Util.fold_init n_segs f []

  let of_path ?closed ?uniform ?size ?tangents path =
    of_bezpath ~n:3 @@ bezpath_of_path ?closed ?uniform ?size ?tangents path

  let closest_point ?(n = 3) ?max_err bez p =
    P.continuous_closest_point ~n_steps:(n * 3) ?max_err bez p
end
