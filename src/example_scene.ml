open Material_types

let get_scene_objects () =
  let main_sphere =
    let center = Vector.make (-0.6) 0. (-1.2) in
    new Sphere.sphere center 0.5 (new diffuse ()) in
  let back_sphere =
    let center = Vector.make 2. 0. (-3.0) in
    new Sphere.sphere center 0.5 (new diffuse ()) in
  let ground_sphere =
    let center = Vector.make 0. (-100.5) (-1.) in
    new Sphere.sphere center 100. (new diffuse ()) in
  [ground_sphere; main_sphere; back_sphere]
