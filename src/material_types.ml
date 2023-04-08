open Vector


class virtual material () = object (_)
  method virtual scatter : Ray.ray -> Vector.vec3 -> Vector.vec3 -> Ray.ray
end

class diffuse () = object (_)
  inherit material ()
  method scatter _ hit normal =
    let rec random_in_unit_sphere () =
      let v = Vector.random (-1.) 1. in
      if (Vector.length_squared v) >= 1. then
        v
      else
        random_in_unit_sphere () in
    let target =
      hit +:
      normal +:
      random_in_unit_sphere () in
    Ray.make hit (target -: hit)
end
