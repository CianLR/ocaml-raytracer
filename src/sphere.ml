open Vector
open Ray

class sphere' center radius = object
  val cen = center
  val rad = radius

  method hit r t_min t_max =
    let oc = (origin r) -: cen in
    let a = length_squared (direction r) in
    let half_b = dot oc (direction r) in
    let c = (length_squared oc) -. (rad *. rad) in
    let descrim = (half_b *. half_b) -. (a *. c) in
    if descrim < 0. then None else
    let sqrt_descrim = Float.sqrt descrim in
    let root_neg = (-.half_b) -. sqrt_descrim in
    if (t_min <= root_neg) && (root_neg <= t_max) then
      Some root_neg
    else
    let root_pos = (-.half_b) +. sqrt_descrim in
    if (t_min <= root_pos) && (root_pos <= t_max) then
      Some root_pos
    else
    None
end

class sphere = sphere'

