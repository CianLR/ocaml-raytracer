open Vector
open Ray

class sphere' center radius = object
  val cen = center
  val rad = radius

  method hit r =
    let oc = (origin r) -: cen in
    let a = length_squared (direction r) in
    let half_b = dot oc (direction r) in
    let c = (length_squared oc) -. (rad *. rad) in
    let descrim = (half_b *. half_b) -. (a *. c) in
    if descrim < 0. then
      -1.0 (* no roots *)
    else
      (* assumes smallest root is the closest *)
      ((-.half_b) -. (Float.sqrt descrim)) /. a
end

class sphere = sphere'

