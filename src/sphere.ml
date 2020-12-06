open Vector
open Ray

class sphere' center radius = object (self)
  val cen = center
  val rad = radius

  method private get_record r t =
    let intersect = at r t in
    let normal = (intersect -: cen) /: rad in
    Hit_record.make r t intersect normal

  method hit r t_min t_max =
    let oc = (origin r) -: cen in
    let a = length_squared (direction r) in
    let half_b = dot oc (direction r) in
    let c = (length_squared oc) -. (rad *. rad) in
    let descrim = (half_b *. half_b) -. (a *. c) in
    if descrim < 0. then None else
    let sqrt_descrim = Float.sqrt descrim in

    let root_neg = ((-.half_b) -. sqrt_descrim) /. a in
    if (t_min <= root_neg) && (root_neg <= t_max) then
      Some (self#get_record r root_neg)
    else

    let root_pos = ((-.half_b) +. sqrt_descrim) /. a in
    if (t_min <= root_pos) && (root_pos <= t_max) then
      Some (self#get_record r root_pos)
    else
    None
end

class sphere = sphere'

