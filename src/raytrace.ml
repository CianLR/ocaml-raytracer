
let closest_hit h1 _ =
  h1

let hit_list r t_min t_max l =
  let maybe_closest a b =
    match a, b with
    | None, None -> None
    | None, Some h -> Some h
    | Some h, None -> Some h
    | Some h1, Some h2 -> Some (closest_hit h1 h2) in
  let hl = List.map (fun h -> h#hit r t_min t_max) l in
  List.fold_left maybe_closest None hl

