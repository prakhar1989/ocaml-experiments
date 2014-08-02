(* CS51 Section 1 *)
(* Goal for today: more practice with Ocaml. In particular, we'll be
   starting to see the power of higher order functions.  

  Agenda:
   * Higher-order functions and polymorphism
   * Map and reduce
*)

(* Map and reduce *)
(* Exercise 1 *)
let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  match xs with
    | [] -> u
    | hd::tl -> f hd (reduce f u tl);;

let rec map (f:'a -> 'b) (xs: 'a list) : 'b list =
  match xs with
    | [] -> []
    | hd::tl -> (f hd) :: (map f tl);;

(* Or: let map = List.map;; *)
(* Or: let reduce f u xs = List.fold_right f xs u *)

(* 1a. Implement length in terms of reduce. 
 * length lst returns the length of lst. length [] = 0. *)
let length (lst: int list) : int = failwith "unimplemented";;





(* Is there a way to write this without the argument: *)
let length' = failwith "unimplemented";;

(* 1b. Write a function that takes an int list and multiplies every int by 3.
 * Use map. *)
let times_3 (lst: int list): int list = failwith "unimplemented";;





(* 1c. Write a function that takes an int list and an int and multiplies every
 * entry in the list by the int. Use map. *)
let times_x (x: int) (lst: int list) : int list = failwith "unimplemented";;





(* 1d. Rewrite times_3 in terms of times_x. 
 * This should take very little code. *)
let times_3_shorter = failwith "unimplemented";;

    
(* 1e. Write a function that takes an int list and generates a "multiplication
 * table", a list of int lists showing the product of any two entries in the
 * list. e.g. mult_table [1;2;3] => [[1; 2; 3]; [2; 4; 6]; [3; 6; 9]] *)
let mult_table (lst: int list) : int list list = failwith "unimplemented";;




(* 1f. Write a function that takes a list of boolean values
 * [x1; x2; ... ; xn] and returns x1 AND x2 AND ... AND xn.
 * For simplicity, assume and_list [] is TRUE. Use reduce. *)
let and_list (lst: bool list) : bool = failwith "unimplemented";;




(* 1g. Do the same as above, with OR.
 * Assume or_list [] is FALSE. *)
let or_list (lst: bool list) : bool = failwith "unimplemented";;



	

(* 1h. Write and_list to return a bool option,
 * where the empty list yields None. Use map and/or reduce.
 * (How would we do this without map?)
 * You may find it helpful to use a function you wrote earlier. *)
let and_list_smarter (lst: bool list) : bool option = failwith "unimplemented";;




	
(* 1i. Return the max of a list, or None if the list is empty. *)
let max_of_list (lst:int list) : int option = failwith "unimplemented";;




(* 1j. Return the min and max of a list, or None if the list is empty. *)
let bounds (lst:int list) : (int option * int option) = 
  failwith "unimplemented";;
