
type vec3 = {
  x : float;
  y : float;
  z: float
}

let make x y z =
  { x=x; y=y; z=z }

let make_int x y z =
  make (float_of_int x) (float_of_int y) (float_of_int z)

let x v = v.x
let y v = v.y
let z v = v.z

let negate v =
  make (-.v.x) (-.v.y) (-.v.z)

let ( +: ) v1 v2 =
  make (v1.x +. v2.x) (v1.y +. v2.y) (v1.z +. v2.z)

let ( -: ) v1 v2 =
  v1 +: (negate v2)

let ( *: ) v t =
  make (v.x *. t) (v.y *. t) (v.z *. t)

let ( /: ) v t =
  make (v.x /. t) (v.y /. t) (v.z /. t)

let dot v1 v2 =
  (v1.x *. v2.x) +. (v1.y *. v2.y) +. (v1.z *. v2.z)

let length_squared v =
  dot v v

let length v =
  Float.sqrt (length_squared v)

let unit_vector v =
  v /: (length v)

let random min max =
  let width = max -. min in
  let rand () = min +. Random.float width in
  {x=rand (); y=rand (); z=rand ()}

let print v =
  Printf.printf "[%.4f %.4f %.4f]\n" v.x v.y v.z
