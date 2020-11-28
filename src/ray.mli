
type ray

val make : Vector.vec3 -> Vector.vec3 -> ray

val origin : ray -> Vector.vec3
val direction : ray -> Vector.vec3

val at : ray -> float -> Vector.vec3
