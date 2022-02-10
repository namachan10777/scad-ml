let sign a = Float.(of_int @@ compare a 0.)
let clamp ~min ~max a = Float.min (Float.max min a) max
let lerp a b u = ((1. -. u) *. a) +. (u *. b)

let lerpn ?(endpoint = true) a b n =
  let d = Float.of_int @@ if endpoint then n - 1 else n in
  List.init n (fun i ->
      let u = Float.of_int i /. d in
      lerp a b u )

let quant ~q v = Float.floor ((v /. q) +. 0.5) *. q
let quant_down ~q v = Float.floor (v /. q) *. q
let quant_up ~q v = Float.ceil (v /. q) *. q
let approx ?(eps = Util.epsilon) a b = Float.(compare (abs (a -. b)) eps) < 1

let law_of_cosines a b c =
  clamp ~min:(-1.) ~max:1.
  @@ Float.acos (((a *. a) +. (b *. b) -. (c *. c)) /. (2. *. a *. b))

let posmod a m = mod_float (mod_float a m +. m) m

let matmul a b =
  let a_rows = Array.length a
  and b_rows = Array.length b in
  let a_cols = if a_rows = 0 then 0 else Array.length a.(0)
  and b_cols = if b_rows = 0 then 0 else Array.length b.(0) in
  if a_cols <> b_rows
  then (
    let msg = Printf.sprintf "matmul: Inner dims do not match (%i x %i)" a_cols b_rows in
    raise (Invalid_argument msg) );
  if not @@ Array.for_all (fun c -> Array.length c = a_cols) a
  then raise (Invalid_argument "matmul: First matrix has ragged columns.");
  if not @@ Array.for_all (fun c -> Array.length c = b_cols) b
  then raise (Invalid_argument "matmul: Second matrix has ragged columns.");
  let out = Array.make_matrix a_rows b_cols 0. in
  for i = 0 to a_rows - 1 do
    for j = 0 to b_cols - 1 do
      for k = 0 to b_rows - 1 do
        out.(i).(j) <- out.(i).(j) +. (a.(i).(k) *. b.(k).(j))
      done
    done
  done;
  out

let poly_trim_head p =
  let i = ref 0
  and first = ref None
  and len = Array.length p in
  while Option.is_none !first && !i < len do
    if p.(!i) = 0. then incr i else first := Some !i
  done;
  match !first with
  | Some first -> Array.init (len - first) (fun i -> p.(i + first))
  | None       -> [| 0. |]

let poly_trim_head_tail p =
  let len = Array.length p in
  let i = ref 0
  and j = ref (len - 1)
  and first = ref None
  and last = ref None in
  while (Option.is_none !first || Option.is_none !last) && !i < len do
    if Option.is_none !first && p.(!i) <> 0. then first := Some !i else incr i;
    if Option.is_none !last && p.(!j) <> 0. then last := Some !j else decr j
  done;
  match !first, !last with
  | Some first, Some last -> Array.init (last - first + 1) (fun i -> p.(i + first))
  | _                     -> [| 0. |]

module type Arithmatic = sig
  type t

  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
end

let complex_of_real re = Complex.{ re; im = 0. }

let polynomial
    (type a)
    ~(of_real : float -> a)
    (module M : Arithmatic with type t = a)
    p
    (z : a)
    : a
  =
  let p = poly_trim_head p in
  let len = Array.length p in
  let rec aux k total =
    if k = len then total else aux (k + 1) M.(add (mul total z) (of_real p.(k)))
  in
  aux 0 M.zero

let polynomial_real = polynomial ~of_real:Fun.id (module Float)
let polynomial_complex = polynomial ~of_real:complex_of_real (module Complex)

(* Adapted from: https://github.com/revarbat/BOSL2/blob/master/math.scad#L1418 *)
let poly_roots ?(tol = 1e-14) p =
  let p = poly_trim_head_tail p in
  if Array.for_all (( = ) 0.) p
  then raise (Invalid_argument "Input polynomial cannot be zero.");
  let n = Array.length p - 1 in
  (* polynomial degree *)
  if n = 0
  then [||], [||]
  else if n = 1
  then [| Complex.{ re = -.p.(1) /. p.(0); im = 0. } |], [| 0. |]
  else (
    let p0 = p.(0)
    and p1 = p.(1) in
    let p_deriv = Array.init n (fun i -> p.(i) *. Float.of_int (n - i)) in
    let s =
      Array.init (n + 1) (fun i ->
          Float.abs p.(i) *. ((4. *. Float.of_int (n - i)) +. 1.) )
    and beta = -.p1 /. p0 /. Float.of_int n in
    let z =
      let r =
        let poly = polynomial_real p beta in
        1. +. Float.(pow (abs (poly /. p0)) (1. /. of_int n))
      in
      let f i =
        let angle = Float.((pi *. 2. *. (of_int i /. of_int n)) +. (1.5 /. of_int n)) in
        Complex.(
          add { re = beta; im = 0. } Float.{ re = cos angle *. r; im = sin angle *. r })
      in
      Array.init n f
    in
    let i = ref 0
    and svals = Array.make n 0.
    and p_of_z = Array.make n Complex.zero
    and complete = Array.make n false
    and n_complete = ref 0
    and z_diff = ref Complex.zero in
    while !n_complete < n && !i < 45 do
      for j = 0 to n - 1 do
        if not complete.(j)
        then (
          svals.(j) <- tol *. polynomial_real s (Complex.norm z.(j));
          p_of_z.(j) <- polynomial_complex p z.(j);
          if Complex.norm p_of_z.(j) <= svals.(j)
          then (
            complete.(j) <- true;
            incr n_complete )
          else (
            let newton = Complex.div p_of_z.(j) (polynomial_complex p_deriv z.(j)) in
            for k = 0 to n do
              if j <> k then z_diff := Complex.(add !z_diff (div one (sub z.(j) z.(k))))
            done;
            let w = Complex.(div newton (sub one (mul newton !z_diff))) in
            z_diff := Complex.zero;
            z.(j) <- Complex.sub z.(j) w ) )
      done;
      incr i
    done;
    if !n_complete < n then raise (Failure "poly_roots exceeded iteration limit.");
    let error =
      let f xi =
        let num =
          Complex.norm (polynomial_complex p xi)
          +. (tol *. polynomial_real s (Complex.norm xi))
        and denom =
          Float.abs
            ( Complex.norm (polynomial_complex p_deriv xi)
            -. (tol *. polynomial_real s (Complex.norm xi)) )
        in
        Float.of_int n *. num /. denom
      in
      Array.map f z
    in
    z, error )

let real_roots ?eps ?(tol = 1e-14) p =
  let p = poly_trim_head p in
  let roots, errors = poly_roots ~tol p in
  let f =
    match eps with
    | Some eps ->
      fun (_, acc) (Complex.{ re; im } as z) ->
        if Float.abs im /. (1. +. Complex.norm z) < eps then 0, re :: acc else 0, acc
    | None     ->
      fun (i, acc) Complex.{ re; im } ->
        if Float.abs im <= errors.(i) then i + 1, re :: acc else i + 1, acc
  in
  let _, l = Array.fold_left f (0, []) roots in
  Util.array_of_list_rev l
