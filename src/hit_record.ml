open Vector
open Ray

type hit_record = {
  t : float;
  hit : vec3;
  normal: vec3;
  front_face: bool;
  material: Material_types.material
}

let make r t hit out_normal material =
  let front_face = (dot (direction r) out_normal) < 0. in
  let norm = if front_face then out_normal else (out_normal *: -1.) in
  {t=t; hit=hit; normal=norm; front_face=front_face; material=material}

let t hr = hr.t
let hit hr = hr.hit
let normal hr = hr.normal
let front_face hr = hr.front_face
let material hr = hr.material
