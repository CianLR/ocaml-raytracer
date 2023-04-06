
type vec3

val make_int : int -> int -> int -> vec3
val make : float -> float -> float -> vec3

val x : vec3 -> float
val y : vec3 -> float
val z : vec3 -> float

val negate : vec3 -> vec3

val ( +: ) : vec3 -> vec3 -> vec3
val ( -: ) : vec3 -> vec3 -> vec3
val ( *: ) : vec3 -> float -> vec3
val ( /: ) : vec3 -> float -> vec3

val dot : vec3 -> vec3 -> float

val length_squared : vec3 -> float
val length : vec3 -> float

val unit_vector : vec3 -> vec3

val random : float -> float -> vec3

val print : vec3 -> unit

