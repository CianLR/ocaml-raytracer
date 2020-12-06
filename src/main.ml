open Vector
open Ray
open Hit_record

let kScale = 2
let kWindowWidth = 640 * kScale
let kWindowHeight = 360 * kScale

let lerp c1 c2 t =
  (c1 *: (1. -. t)) +: (c2 *: t)

let vec_to_rgb v =
  Graphics.rgb
    (int_of_float (255. *. (x v)))
    (int_of_float (255. *. (y v)))
    (int_of_float (255. *. (z v)))

let get_scene_objects () =
  let main_sphere =
    let center = Vector.make 0. 0. (-1.) in
    new Sphere.sphere center 0.5 in
  let ground_sphere =
    let center = Vector.make 0. (-100.5) (-1.) in
    new Sphere.sphere center 100. in
  [main_sphere; ground_sphere]

let ray_color scene r =
  match Raytrace.hit_list r 0.0 Float.infinity scene with
  | Some record ->
    ((normal record) +: (Vector.make 1. 1. 1.)) *: 0.5
    |> vec_to_rgb
  | None ->
    let unit_direction = unit_vector (direction r) in
    let t = 0.5 *. (1.0 +. y unit_direction) in
    lerp (Vector.make 1. 1. 1.) (Vector.make 0. 0. 0.) t
    |> vec_to_rgb

let draw_scene () =
  let cam = Camera.make () in
  let scene = get_scene_objects () in
  for y = 0 to kWindowHeight - 1 do
    for x = 0 to kWindowWidth - 1 do
      let u = (float_of_int x) /. (float_of_int (kWindowWidth - 1)) in
      let v = (float_of_int y) /. (float_of_int (kWindowHeight - 1)) in
      let ray = Camera.get_ray cam u v in
      Graphics.set_color (ray_color scene ray);
      Graphics.plot x y
    done
  done

let () =
  let graph_spec = Printf.sprintf " %dx%d" kWindowWidth kWindowHeight in
  Graphics.open_graph graph_spec;
  Graphics.auto_synchronize false;
  draw_scene ();
  Graphics.synchronize ();
  let status = Graphics.wait_next_event [Graphics.Key_pressed] in
  Printf.printf "%d %d\n" status.mouse_x status.mouse_y

