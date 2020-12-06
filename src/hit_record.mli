
type hit_record

val make : Ray.ray -> float -> Vector.vec3 -> Vector.vec3 -> hit_record

val t : hit_record -> float
val hit : hit_record -> Vector.vec3
val normal : hit_record -> Vector.vec3
val front_face : hit_record -> bool

