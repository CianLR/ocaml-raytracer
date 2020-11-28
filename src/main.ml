open Vector
open Ray

let kWindowWidth = 640
let kWindowHeight = 360

let hit_sphere cen rad r =
  let oc = (origin r) -: cen in
  let a = dot (direction r) (direction r) in
  let b = 2. *. (dot oc (direction r)) in
  let c = (dot oc oc) -. (rad *. rad) in
  let descrim = (b *. b) -. (4. *. a *. c) in
  descrim > 0.

let get_lower_left o h v focal =
  ((o -: (h /: 2.)) -: (v /: 2.)) -: (Vector.make 0. 0. focal)

let lerp c1 c2 t =
  (c1 *: (1. -. t)) +: (c2 *: t)

let vec_to_rgb v =
  Graphics.rgb
    (int_of_float (255. *. (x v)))
    (int_of_float (255. *. (y v)))
    (int_of_float (255. *. (z v)))

let ray_color r =
  if hit_sphere (Vector.make 0. 0. (-1.)) 0.5 r then
    Vector.make 0. 1. 0. |> vec_to_rgb
  else
  let unit_direction = unit_vector (direction r) in
  let t = 0.5 *. (1.0 +. y unit_direction) in
  lerp (Vector.make 1. 1. 1.) (Vector.make 1. 0. 0.) t
  |> vec_to_rgb

let draw_scene () =
  (* TODO: Calc from width/height *)
  let aspect_ratio = 16.0 /. 9.0 in
  let viewport_height = 2.0 in
  let viewport_width = viewport_height *. aspect_ratio in
  let focal_length = 1.0 in
  let origin = Vector.make 0. 0. 0. in
  let horizontal = Vector.make viewport_width 0. 0. in
  let vertical = Vector.make 0. viewport_height 0. in
  let llc = get_lower_left origin horizontal vertical focal_length in
  for y = 0 to kWindowHeight - 1 do
    for x = 0 to kWindowWidth - 1 do
      let u = (float_of_int x) /. (float_of_int (kWindowWidth - 1)) in
      let v = (float_of_int y) /. (float_of_int (kWindowHeight - 1)) in
      let dir = (llc +: (horizontal *: u) +: (vertical *: v)) -: origin in
      let ray = Ray.make origin dir in
      Graphics.set_color (ray_color ray);
      Graphics.plot x y
    done
  done

let () =
  let graph_spec = Printf.sprintf " %dx%d" kWindowWidth kWindowHeight in
  Graphics.open_graph graph_spec;
  draw_scene ();
  ignore (Graphics.wait_next_event [Graphics.Key_pressed]);
  print_newline ();

