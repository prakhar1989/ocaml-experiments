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

(* imperative programming *)
let numbers = [| 1; 2; 3; 4 |]

(* mutable record fields *)
type running_sum = 
  {
    mutable sum: float;
    mutable sum_sq: float;
    mutable samples: int;
  }

let mean rsum = rsum.sum /. float rsum.samples
let stdev rsum = 
  sqrt (rsum.sum_sq /. float rsum.samples
          -. (rsum.sum /. float rsum.samples) ** 2.)

let create () = { sum = 0.; sum_sq = 0.; samples = 0 }
let update rsum x = 
   rsum.samples <- rsum.samples + 1;
   rsum.sum     <- rsum.sum     +. x;
   rsum.sum_sq  <- rsum.sum_sq  +. x *. x

(* refs *)
let x = ref 0

let sum_imperative list =
  let sum = ref 0 in
  List.iter list ~f:(fun x -> sum := !sum + x);
  !sum

let permute array = 
  let length = Array.length array in 
  for i = 0 to length - 2 do
    let j = i + 1 + Random.int (length - i - 1) in 
    let tmp = array.(i) in 
    array.(i) <- array.(j);
    array.(j) <- tmp
  done

(* main function *)
let () =
  let p = { x = 3.3 ; y = 4.5 } in
    printf "All in point: %B\n" (is_inside_scene p [circle; line; rect]);;

  let rsum = create () in
    List.iter [1.;3.;2.;-7.;4.;5.] ~f:(fun x -> update rsum x);
    printf "Sum: %F\n" (rsum.sum);;
  
  printf "Reference X: %d\n" (!x + 1);;

