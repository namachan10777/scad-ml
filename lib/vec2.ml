type t = float * float

let zero = 0., 0.
let horizontal_op op (x1, y1) (x2, y2) = op x1 x2, op y1 y2
let add = horizontal_op ( +. )
let sub = horizontal_op ( -. )
let mul = horizontal_op ( *. )
let div = horizontal_op ( /. )
let negate = horizontal_op ( *. ) (-1., -1.)
let map f (x, y) = f x, f y
let add_scalar t s = map (( +. ) s) t
let sub_scalar t s = sub t (s, s)
let mul_scalar t s = map (( *. ) s) t
let div_scalar t s = div t (s, s)

let equal a b =
  match horizontal_op ( = ) a b with
  | true, true -> true
  | _          -> false

let norm (x, y) = Float.sqrt ((x *. x) +. (y *. y))
let distance a b = norm (sub a b)

let normalize ((x, y) as t) =
  let n = norm t in
  if n > 0. then x /. n, y /. n else t

let dot (x1, y1) (x2, y2) = (x1 *. x2) +. (y1 *. y2)

let mean l =
  let n, sum = List.fold_left (fun (i, s) t -> i + 1, add t s) (0, zero) l in
  map (fun a -> a /. Float.of_int n) sum

let lerp a b u = add (mul_scalar a (1. -. u)) (mul_scalar b u)

let lerpn ?(endpoint = true) a b n =
  let d = Float.of_int @@ if endpoint then n - 1 else n in
  List.init n (fun i ->
      let u = Float.of_int i /. d in
      lerp a b u )

let get_x (x, _) = x
let get_y (_, y) = y
let to_string (x, y) = Printf.sprintf "[%f, %f]" x y
let deg_of_rad t = map (fun r -> 180.0 *. r /. Float.pi) t
let rad_of_deg t = map (fun d -> d *. Float.pi /. 180.) t
let ( <+> ) = add
let ( <-> ) = sub
let ( <*> ) = mul
let ( </> ) = div
let of_vec3 (x, y, _) = x, y
let to_vec3 ?(z = 0.) (x, y) = x, y, z

let rotate theta (x, y) =
  let s = Float.sin theta
  and c = Float.cos theta in
  let x' = (x *. c) -. (y *. s)
  and y' = (y *. c) +. (x *. s) in
  x', y'

let rotate_about_pt r pivot t = add t pivot |> rotate r |> add (negate pivot)
let translate = add
let scale = mul
let mirror ax t = sub t (mul_scalar ax (2. *. (dot t ax /. dot ax ax)))
