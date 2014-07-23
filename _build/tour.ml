open Core.Std

let rec sum l = 
  match l with 
  | [] -> 0
  | hd :: tl -> hd + sum tl

let divide x y = 
  if y = 0 then None else Some (x/y)

let log_entry maybe_time message = 
  let time = 
    match maybe_time with
    | Some x -> x
    | None -> Time.now()
  in
  Time.to_sec_string time ^ " -- " ^ message

let t = log_entry (Some Time.epoch) "a log message"

(* record type *)
type point2d = { x: float; y: float; }

(* pattern matching for record type *)
let magnitude { x = x_pos; y = y_pos } = 
  sqrt (x_pos ** 2. +. y_pos ** 2.)

let distance v1 v2 = 
  magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y }

 (* composite types *)
type circle_desc = { center: point2d; radius: float }
type rect_desc = { lower_left: point2d; height: float; width: float }
type line_desc = { endpoint1: point2d; endpoint2: point2d}

(* variant types *)
type scene_element = 
  | Circle    of circle_desc
  | Rectangle of rect_desc
  | Line      of line_desc

let is_inside_scene_element point scene_element = 
  match scene_element with 
  | Circle    { center; radius } -> 
      distance center point < radius
  | Rectangle { lower_left; height; width } ->
      point.x    > lower_left.x && point.x < lower_left.x +. width
      && point.y > lower_left.y && point.y < lower_left.y +. height
  | Line { endpoint1; endpoint2 } -> false

let circle = Circle { center={x=3.;y=4.}; radius=3.}
let line = Line { endpoint1={x=1.;y=1.}; endpoint2={x=2.;y=2.}}
let rect = Rectangle { lower_left={x=5.;y=6.}; height=3.; width=7.}

(* anon functions *)
let is_inside_scene point scene = 
  List.exists scene ~f:(fun el -> is_inside_scene_element point el)

let () =
  let p = { x = 3.3 ; y = 4.5 } in
    printf "Circle in point: %B\n" (is_inside_scene_element p circle);;

  let p = { x = 3.3 ; y = 4.5 } in
    printf "All in point: %B\n" (is_inside_scene p [circle; line; rect]);;

