open Vector
open Ray

module T = Domainslib.Task

let kScale = 2
let kWindowWidth = 640 * kScale
let kWindowHeight = 360 * kScale
let kSamplesPerPixel = 50
let kMaxReflectionDepth = 10

let lerp c1 c2 t =
  (c1 *: (1. -. t)) +: (c2 *: t)

let vec_to_rgb v =
  Graphics.rgb
    (int_of_float (255. *. (Float.sqrt (x v))))
    (int_of_float (255. *. (Float.sqrt (y v))))
    (int_of_float (255. *. (Float.sqrt (z v))))

let get_scene_objects () =
  let main_sphere =
    let center = Vector.make (-0.6) 0. (-1.2) in
    new Sphere.sphere center 0.5 in
  let back_sphere =
    let center = Vector.make 2. 0. (-3.0) in
    new Sphere.sphere center 0.5 in
  let ground_sphere =
    let center = Vector.make 0. (-100.5) (-1.) in
    new Sphere.sphere center 100. in
  [ground_sphere; main_sphere; back_sphere]

let rec random_in_unit_sphere () =
  let v = Vector.random (-1.) 1. in
  if (Vector.length_squared v) >= 1. then
    v
  else
    random_in_unit_sphere ()

let rec ray_color scene r depth =
  if depth = 0 then Vector.make 0. 0. 0. else
  match Raytrace.hit_list r 0.0001 Float.infinity scene with
  | Some record ->
    let hit = Hit_record.hit record in
    let target =
      hit +:
      Hit_record.normal record +:
      random_in_unit_sphere () in
    let ray = Ray.make hit (target -: hit) in
    (ray_color scene ray (depth - 1)) *: 0.5
  | None ->
    let unit_direction = unit_vector (direction r) in
    let t = 0.5 *. (1.0 +. y unit_direction) in
    lerp (Vector.make 1. 1. 1.) (Vector.make 0.5 0.7 1.0) t

let sample_pixel scene cam samples x y =
  let fuzz_points () =
    ((float_of_int x) +. ((Random.float 2.0) -. 1.0),
     (float_of_int y) +. ((Random.float 2.0) -. 1.0)) in
  let scale_points (x, y) =
    (x /. (float_of_int (kWindowWidth - 1)),
     y /. (float_of_int (kWindowHeight - 1))) in
  let points_to_color (u, v) =
    ray_color scene (Camera.get_ray cam u v) kMaxReflectionDepth in
  List.init samples (fun _ -> fuzz_points ())
  |> List.map scale_points
  |> List.map points_to_color
  |> List.fold_left (+:) (Vector.make 0. 0. 0.)
  |> fun v -> v /: (float_of_int samples)
  |> vec_to_rgb

let draw_scene pool pixels ?(aa=true) () =
  let samples = if aa then kSamplesPerPixel else 1 in
  let cam = Camera.make kWindowHeight kWindowWidth in
  let scene = get_scene_objects () in
  let pixel_count = (kWindowHeight * kWindowWidth) in
  T.parallel_for pool ~start:0 ~finish:(pixel_count - 1) ~body:(fun p ->
    let x = p mod kWindowWidth in
    let y = p / kWindowWidth in
    let color = sample_pixel scene cam samples x y in
    pixels.(kWindowHeight - y - 1).(x) <- color;
  )

let write_pixels pixels =
  let img = Graphics.make_image pixels in
  Graphics.draw_image img 0 0

let () =
  let graph_spec = Printf.sprintf " %dx%d" kWindowWidth kWindowHeight in
  Graphics.open_graph graph_spec;
  Graphics.moveto (kWindowWidth / 2) (kWindowHeight / 2);
  Graphics.set_font "-*-*-*-*-*-*-20-*-*-*-*-*-*-*";
  Graphics.draw_string "Loading...";
  let pool = T.setup_pool ~num_domains:4 () in
  (* Graphics.auto_synchronize false; *)
  let pixels = Array.make_matrix kWindowHeight kWindowWidth Graphics.background in
  T.run pool (draw_scene pool pixels);
  T.teardown_pool pool;
  write_pixels pixels;
  (* draw_scene (); *)
  (* Graphics.synchronize (); *)
  let status = Graphics.wait_next_event [Graphics.Key_pressed] in
  Printf.printf "%d %d\n" status.mouse_x status.mouse_y
