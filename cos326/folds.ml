(* equivalent to List.fold_right in the List module *)
let rec fold_right (f:'a -> 'b -> 'b) (xs:'a list) (base:'b) : 'b =
  match xs with
    | [] -> base
    | hd::tail -> f hd (fold_right f tail base)
;;

(* reduce from lecture is the same as fold_right with the arguments flipped *)
(* arguments in this order are typically better for partial application *)
let reduce f base xs = fold_right f xs base;;

(* equivalent to List.fold_left in the List module *)
(* more efficient than fold_right (tail recursive) *)
let rec fold_left (f:'b -> 'a -> 'b) (base:'b) (xs:'a list) : 'b =
  match xs with
    | [] -> base
    | hd::tail -> fold_left f (f base hd) tail
;;

(* Simple tests *)

let eg1 = [1;2;3;4];;
let eg2 = [];;

(* Challenges *)
		 
(* takes a list and returns the same list: use a fold *)
let id xs = List.fold_right ~init:[] ~f:(fun x acc -> x :: acc)

(* takes a list and returns the reverse: use a fold *)
let reverse xs = List.fold_left ~init:[] ~f:(fun acc x -> x :: acc)

(* write the polymorphic map from lecture using fold *)
let map f xs = failwith "unimplemented";;
 
(* write a function that maps f across a list and reverses the result *)
let map_rev f xs = failwith "unimplemented";;

(* write a function that flattens a list of lists in to a single
 * list with all of the elements in order. eg:
 *
 * flatten [[1;2;3]; []; [4]; [5;6]] == [1;2;3;4;5;6] 
 *
 * Write the function 2 ways: 
 * 1. with recurseion
 * 2. without
 *
 *)

(* with recursion *)

let rec flatten (xss:'a list list) : 'a list =
  failwith "unimplemented"
;;


(* without recursion, using folds *)

let flattenF (xss:'a list list) : 'a list =
  failwith "unimplemented"
;;

let t1 = [[1;2;3]; [4;5]];;
