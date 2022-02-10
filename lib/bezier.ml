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

  val bez : vec list -> float -> vec
  val curve : ?init:vec list -> ?rev:bool -> ?fn:int -> (float -> vec) -> vec list
  val travel : ?start_u:float -> ?end_u:float -> ?max_deflect:float -> vec list -> float
  val patch : vec list list -> float -> float -> vec

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
    -> vec list
end

module Make (V : Sigs.Vec) : S with type vec := V.t = struct
  module P = Path.Make (V)

  let bez ps =
    let ps = Array.of_list ps in
    let n = Array.length ps - 1 in
    let bm = bezier_matrix n
    and m = Array.make (n + 1) V.zero in
    for i = 0 to n do
      let row = bm.(i) in
      for j = 0 to n do
        m.(i) <- V.add m.(i) (V.mul_scalar ps.(j) row.(j))
      done
    done;
    fun u ->
      let pt = ref V.zero in
      for i = 0 to n do
        pt := V.add !pt (V.mul_scalar m.(i) (Float.pow u (Float.of_int i)))
      done;
      !pt

  let curve ?(init = []) ?(rev = false) ?(fn = 16) bez =
    let dt = 1. /. Float.of_int fn *. if rev then 1. else -1. in
    let rec loop acc i t =
      if i <= fn then loop (bez t :: acc) (i + 1) (t +. dt) else acc
    in
    loop init 0 (if rev then 0. else 1.)

  let travel ?(start_u = 0.) ?(end_u = 1.) ?(max_deflect = 0.01) ps =
    let n_segs = List.length ps * 2
    and bz = bez ps in
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
          let mid_point = V.div_scalar (V.add (p i) (p (i + 2))) 2. in
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
    let horizontal_bezs = List.map bez grid in
    fun u ->
      let vertical_bez = bez @@ List.map (fun bz -> bz u) horizontal_bezs in
      vertical_bez

  let of_path ?(closed = false) ?(uniform = false) ?(size = `FlatRel 0.1) ?tangents path =
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
      | _ -> raise (Invalid_argument "Size must be greater than zero.")
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
      let seg_vec = V.(div_scalar (sub p2 p1) seg_len)
      and t1 = tangents.(i) (* second tangent pointing backwards *)
      and t2 = V.negate tangents.(Util.index_wrap ~len:len_ts (i + 1)) in
      (* total component of tangents parallel to the segment *)
      let parallel = Float.abs (V.dot t1 seg_vec) +. Float.abs (V.dot t2 seg_vec)
      and normal1 = V.(sub t1 (mul_scalar seg_vec (dot t1 seg_vec)))
      and normal2 = V.(sub t2 (mul_scalar seg_vec (dot t2 seg_vec))) in
      let p = Array.map (fun a -> a.(0)) (power_poly normal1 normal2) in
      let uextreme =
        let p_norm = Float.sqrt (Array.fold_left (fun sum a -> (a *. a) +. sum) 0. p) in
        if Math.approx p_norm 0.
        then [||]
        else (
          let f acc r = if r > 0. && r < 1. then r :: acc else acc in
          Util.array_of_list_rev (Array.fold_left f [] (Math.real_roots p)) )
      in
      let dists =
        let bz = bez [ V.zero; normal1; normal2; V.zero ] in
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
      V.(add p2 (mul_scalar t2 l)) :: V.(add p1 (mul_scalar t1 l)) :: p1 :: acc
    in
    ps.(len_ps - 1) :: Util.fold_init len_ts f [] |> List.rev
  (* TODO: feed into a bezpath *)
end
