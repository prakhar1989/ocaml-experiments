let bool_to_int (b:bool) : int =
  match b with
      true  -> 1
    | false -> 0
;;

(* (first name, last name, age) *)
type info = string * string * int;;
let name_and_age1 : info = ("David", "Walker", 25);; 
let name_and_age2 : info = ("Brian", "Kernighan", 15);; 

let full_name ((first, last, _) : info) : string = first ^ " " ^ last;;

let or_pair (p:bool*bool) : bool = 
  match p with
    | (true,true) -> true
    | (true,false) -> true
    | (false,true) -> true
    | (false,false) -> false
;;

(* count occurrences of true *)
let count5 (p:bool*bool*bool*bool*bool) : int = 
  let (b1, b2, b3, b4, b5) = p in
  bool_to_int b1 + 
    bool_to_int b2 + 
    bool_to_int b3 + 
    bool_to_int b4 + 
    bool_to_int b5
;;

(* in_range:
 *   true,  if x is (Some f) and f is between low and high, inclusive
 *   false, otherwise *)
let in_range (x: float option) (low:float) (high:float) =
  match x with
      None -> false
    | Some f -> (low <= f & f <= high)
;;

(* (x,y) coordinates of a point *)
type point = int * int;;

(* slope of a line *)
let slope (p1:point) (p2:point) : float option = 
  let (x1,y1) = p1 in 
  let (x2,y2) = p2 in 
  let xd = x2 - x1 in 
  let yd = y2 - y1 in 
  if xd != 0 then 
    Some ( float_of_int yd /. float_of_int xd ) 
  else 
    None
;;

(* the sum of 0..n;  n must be a natural number *)
let rec sum_to (n:int):int = 
  assert(0 <= n);
  match n with
    0 -> 0
  | _ -> n + sum_to (n-1) 
;;

(***********)
(* Testing *)
(***********)

assert (bool_to_int true = 1);
assert (bool_to_int false = 0);

assert(full_name name_and_age1 = "David Walker");
assert(full_name name_and_age2 = "Brian Kernighan");

assert(or_pair (true,true) = true);
assert(or_pair (true,false) = true);
assert(or_pair (false,true) = true);
assert(or_pair (false,false) = false);

assert(in_range (Some 0.0) (-1.0) (2.0));
assert(in_range (Some 0.0) (0.0) (0.0));
assert(not (in_range (Some 3.0) (5.0) (-2.0)));
assert(not (in_range (None) (-100.0) (200.0)));

assert(slope (0,0) (0,0) = Some 3.5);
assert(slope (1,17) (1,-15) = None);
assert(in_range (slope (0,0) (1,0)) (-0.0001) 0.0001);
assert(in_range (slope (0,0) (1,1)) (0.9999) 1.0001);

assert (sum_to 0 = 0);
assert (sum_to 3 = 6);
;;
