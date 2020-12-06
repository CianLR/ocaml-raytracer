

val hit_list :
  Ray.ray -> float -> float ->
  < hit : Ray.ray -> float ->
          float -> Hit_record.hit_record option; .. > list ->
  Hit_record.hit_record option

