type t = {
  x : float;
  y : float;
  z : float;
}

let string_of_vector { x; y; z } =
  Printf.sprintf "{ x = %f; y = %f; z = %f }" x y z

let vector0 = { x = 0.0; y = 0.0; z = 0.0 }

let pi = atan 1.0 *. 4.0

let cross3 (a, b, c) (d, e, f) =
  (b *. f -. c *. e, 
   c *. d -. a *. f, 
   a *. e -. b *. d)

let cross3' { x = a; y = b; z = c } { x = d; y = e; z = f } =
  { x = b *. f -. c *. e;
    y = c *. d -. a *. f;
    z = a *. e -. b *. d }

let abs3' { x; y; z } = 
  sqrt (x ** 2.0 +. y ** 2.0 +. z ** 2.0)

let (-.|) (x1, x2, x3) (y1, y2, y3) =
  (x1 -. y1, x2 -. y2, x3 -. y3)     

let ( -.|. ) { x = x1; y = x2; z = x3 } { x = y1; y = y2; z = y3 } =
  { x = x1 -. y1; y = x2 -. y2; z = x3 -. y3 }

let (+.|) (x1, x2, x3) (y1, y2, y3) =
  (x1 +. y1, x2 +. y2, x3 +. y3)     

let ( +.|. ) { x = x1; y = x2; z = x3 } { x = y1; y = y2; z = y3 } =
  { x = x1 +. y1; y = x2 +. y2; z = x3 +. y3 }

let ( *.| ) (x1, y1, z1) (x2, y2, z2) =
  (x1 *. x2, y1 *. y2, z1 *. z2)

let ( *.|. ) { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
  { x = x1 *. x2; y = y1 *. y2; z = z1 *. z2 }

let unit3' ({ x; y; z } as v) =
  if x <> 0.0 || y <> 0.0 || z <> 0.0 then
    let l = abs3' v in
    v *.|. { x = 1.0 /. l; y = 1.0 /. l; z = 1.0 /. l }
  else
    vector0
