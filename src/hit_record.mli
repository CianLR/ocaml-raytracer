
type hit_record

val make : Ray.ray -> float -> Vector.vec3 -> Vector.vec3 -> Material_types.material -> hit_record

val t : hit_record -> float
val hit : hit_record -> Vector.vec3
val normal : hit_record -> Vector.vec3
val front_face : hit_record -> bool
val material : hit_record -> Material_types.material
