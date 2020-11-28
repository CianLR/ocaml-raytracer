open Vector

let kWindowWidth = 640
let kWindowHeight = 360

let get_lower_left o h v focal =
  ((o -$ (h /$ 2.)) -$ (v /$ 2.)) -$ (make 0. 0. focal)

let draw_scene () =
  (* TODO: Calc from width/height *)
  let aspect_ratio = 16.0 /. 9.0 in
  let viewport_height = 2.0 in
  let viewport_width = viewport_height *. aspect_ratio in
  let focal_length = 1.0 in
  let origin = make 0. 0. 0. in
  let horizontal = make viewport_width 0. 0. in
  let vertical = make 0. viewport_height 0. in
  let llc = get_lower_left origin horizontal vertical focal_length in
  Printf.printf "%f %f %f\n" (x llc) (y llc) (z llc);
  for y = 0 to kWindowHeight - 1 do
    for x = 0 to kWindowWidth - 1 do
      let r = (256 * x) / kWindowWidth in
      let g = (256 * y) / kWindowHeight in
      let b = 64 in
      Graphics.set_color (Graphics.rgb r g b);
      Graphics.plot x y
    done
  done

let () =
  let graph_spec = Printf.sprintf " %dx%d" kWindowWidth kWindowHeight in
  Graphics.open_graph graph_spec;
  draw_scene ();
  let e = Graphics.wait_next_event [Graphics.Key_pressed] in
  Printf.printf "%c\n" e.key

