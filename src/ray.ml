open Vector

type ray = {
  orig : vec3;
  dir : vec3
}

let make origin direction =
  {orig=origin; dir=direction}

let origin r =
  r.orig

let direction r =
  r.dir

let at r t =
  r.orig +: (r.dir *: t)

