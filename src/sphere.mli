
class sphere : Vector.vec3 -> float -> Material_types.material -> object
    method hit : Ray.ray -> float -> float -> Hit_record.hit_record option
end

