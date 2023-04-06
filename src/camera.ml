open Vector

type camera  = {
  aspect_ratio : float;
  viewport_height : float;
  viewport_width : float;
  focal_length : float;
  origin : Vector.vec3;
  horizontal : Vector.vec3;
  vertical : Vector.vec3;
  lower_left : Vector.vec3
}

let get_lower_left o h v focal =
  ((o -: (h /: 2.)) -: (v /: 2.)) -: (Vector.make 0. 0. focal)

let make height width =
  let aspect_ratio = (float_of_int width) /. (float_of_int height) in 
  let viewport_height = 2.0 in
  let viewport_width = viewport_height *. aspect_ratio in
  let focal_length = 1.0 in
  let origin = Vector.make 0. 0. 0. in
  let horizontal = Vector.make viewport_width 0. 0. in
  let vertical = Vector.make 0. viewport_height 0. in
  let llc = get_lower_left origin horizontal vertical focal_length in
  {
    aspect_ratio=aspect_ratio;
    viewport_height=viewport_height;
    viewport_width=viewport_width;
    focal_length=focal_length;
    origin=origin;
    horizontal=horizontal;
    vertical=vertical;
    lower_left=llc
  }

let get_ray c u v =
  let dir =
    let h = c.horizontal *: u in
    let v = c.vertical *: v in
    (c.lower_left +: h +: v) -: c.origin in
  Ray.make c.origin dir

