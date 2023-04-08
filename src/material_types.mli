
class virtual material : unit -> object
    method virtual scatter : Ray.ray -> Vector.vec3 -> Vector.vec3 -> Ray.ray
end

class diffuse : unit -> object
    method scatter : Ray.ray -> Vector.vec3 -> Vector.vec3 -> Ray.ray
end
