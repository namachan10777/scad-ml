let quad_weights t =
  let a = (1. -. t) ** 2.
  and b = 2. *. t *. (1. -. t)
  and c = t ** 2. in
  a, b, c

let cubic_weights t =
  let a = (1. -. t) ** 3.
  and b = 3. *. t *. ((1. -. t) ** 2.)
  and c = 3. *. (t ** 2.) *. (1. -. t)
  and d = t ** 3. in
  a, b, c, d

let quad ~p1 ~p2 ~p3 t =
  let a, b, c = quad_weights t in
  (a *. p1) +. (b *. p2) +. (c *. p3)

let cubic ~p1 ~p2 ~p3 ~p4 t =
  let a, b, c, d = cubic_weights t in
  (a *. p1) +. (b *. p2) +. (c *. p3) +. (d *. p4)

let quad_vec2 ~p1:(x1, y1) ~p2:(x2, y2) ~p3:(x3, y3) t =
  let a, b, c = quad_weights t in
  let x = (a *. x1) +. (b *. x2) +. (c *. x3)
  and y = (a *. y1) +. (b *. y2) +. (c *. y3) in
  x, y

let cubic_vec2 ~p1:(x1, y1) ~p2:(x2, y2) ~p3:(x3, y3) ~p4:(x4, y4) t =
  let a, b, c, d = cubic_weights t in
  let x = (a *. x1) +. (b *. x2) +. (c *. x3) +. (d *. x4)
  and y = (a *. y1) +. (b *. y2) +. (c *. y3) +. (d *. y4) in
  x, y

let quad_vec3 ~p1:(x1, y1, z1) ~p2:(x2, y2, z2) ~p3:(x3, y3, z3) t =
  let a, b, c = quad_weights t in
  let x = (a *. x1) +. (b *. x2) +. (c *. x3)
  and y = (a *. y1) +. (b *. y2) +. (c *. y3)
  and z = (a *. z1) +. (b *. z2) +. (c *. z3) in
  x, y, z

let cubic_vec3 ~p1:(x1, y1, z1) ~p2:(x2, y2, z2) ~p3:(x3, y3, z3) ~p4:(x4, y4, z4) t =
  let a, b, c, d = cubic_weights t in
  let x = (a *. x1) +. (b *. x2) +. (c *. x3) +. (d *. x4)
  and y = (a *. y1) +. (b *. y2) +. (c *. y3) +. (d *. y4)
  and z = (a *. z1) +. (b *. z2) +. (c *. z3) +. (d *. z4) in
  x, y, z

let curve ?(init = []) ?(rev = false) ~n_steps bez =
  let dt = 1. /. Float.of_int n_steps *. if rev then 1. else -1. in
  let rec loop acc i t =
    if i <= n_steps then loop (bez t :: acc) (i + 1) (t +. dt) else acc
  in
  loop init 0 (if rev then 0. else 1.)
