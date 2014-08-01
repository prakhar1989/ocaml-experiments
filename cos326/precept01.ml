open Core.Std

(* Exercise 1 *)
(* 1a. Make it so that that x equals 42, by adding 22 to 20 *)
let x = 22 + 20

(* 1b. Make it so that x1 equals 42.0, by adding 2 numbers. *)
let x1 = 22.0 +. 20.0


(* 1c. Write a function takes a string, and appends
 * ", and that is why I love cos326" to the end of it. *)
let cos326_loveifier input = 
  input ^ ", and that is why I love cos326"


(* 1d. Write a function that takes a number and returns
 * the difference between that number and 42.
 * Eg, if 'num' is 50, the result should be 8.
 * If 'num' is 30, the result should be -12 *)
let difference_between_num_and_42 num =  num - 42


(* 1e. One more simple arithmetic example...
 * Write a function that returns the volume of a cylinder
 * with height h, radius r. *)
let volume_cylinder (h:float) (r:float) : float = 
  let pi = 4. *. atan 1. in (* tan 1 radian = pi / 4 *)
    pi *. r *. r *. h


(* 1f. Here, you might have a solution in mind, but not know how to
 * implement it in OCaml.  See if you can Google for how to do it... *)
let even (x: int) : bool = x mod 2 = 0

(* 1g. Can you write odd /in terms of/ even? *)
let odd (x: int) : bool = not (even x)

(* 1h. OCaml comes pre-packaged with a standard library, that includes
 * a lot of utility functions that you don't have to write yourself.
 * For instance, check out the String module
 * (http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html)
 *
 * Now... write a function that takes a String, and returns whether
 * or not that String is more than 10 characters long. *)

let is_more_than_10_characters_long str : bool = String.length str > 10

(* Exercise 2 *)

(* 2a. Why doesn't the expression below type check? What small change
 * can you make to it so that it does *)
let rec sumTo (n:int) : int =
 if n <= 0 then 0
 else n + sumTo (n-1)

(* 2b. Make th following typecheck *)
let rec foo (x:char) : char =
  foo ( if true then foo x else 'h')


let rec baz (y:bool) (z:bool) : bool =
  baz (baz z y) (not y)

let f (x: bool): bool = x
let rec bar (y:bool) (x:char) : char = 
  bar (f y) (bar y x)


(* 2c. Type check - Trick Question *)

(*
Let forever (x ??) : ?? =
  forever forever
;; 
*)

type employee = { name: string;  married: bool; age: int }
(* 2d. Make a function that makes a string * int * bool into an employee 
 * and vice versa  *)
 
let tup_to_rec (tup: string * bool * int) : employee = 
  let (name, married, age) = tup in
  { name = name; married = married; age = age}

let rec_to_tup {name=name; married=married; age=age}: (string * bool * int) =
  (name, married, age)

(* 2e. Why doesn't the expression below type check? What small change
 * can you make to it so that it does *)

type employer = {name:string; est_year:int };;
let g : employee = tup_to_rec ("Greg", true, 12);;
g.name = "Ada";;

(* Options *)
(* Exercise 3 *)

(* 3a. Write a function to return the smaller of two int options, or None
 * if both are None. If exactly one argument is None, return the other. *)

let min_option (x: int option) (y: int option) : int option =
  match (x, y) with
  | (None, None) -> None
  | (a, None) -> a
  | (None, b) -> b
  | (a, b) -> if a < b then a else b

	
(* 3b. Write a function that returns the integer buried in the argument
 * or None otherwise *) 

let get_option (x: int option option option option) : int option =
  match x with 
  | None                        -> None
  | Some None                   -> None
  | Some (Some None)            -> None
  | Some (Some (Some None))     -> None
  | Some (Some (Some (Some a))) -> Some a

(* What's the pattern? How can we factor out similar code? *)
(* 3c. Write a higher-order function for binary operations on options.
 * If both arguments are None, return None.  If one argument is (Some x)
 * and the other argument is None, function should return (Some x) *)
(* What is calc_option's function signature? *)

let calc_option (f: int -> int -> int) (x: int option) (y: int option) : int option =
  match (x, y) with
  | (None, None) -> None
  | (a, None) | (None, a) -> a
  | (Some a, Some b) -> Some (f a b)

(* 3d. Now rewrite min_option and max_option using the higher-order function. 
   The built-in functions min and max may be useful *)
let min_option_2 (x: int option) (y: int option) : int option =
  calc_option min x y

(* Can we use this in other ways? *)
(* 3e. Write a function to return the boolean AND of two bool options,
 * or None if both are None. If exactly one is None, return the other. *)
let and_option (x:bool option) (y: bool option) : bool option =
  match (x, y) with 
  | (None, None) -> None
  | (a, None) | (None, a) -> a
  | (Some a, Some b) -> Some (a && b)

(* 4. Compute the GCD for two integers using Euclid's recursion
 * http://en.wikipedia.org/wiki/Greatest_common_divisor *)
let rec gcd (x : int) (y : int) : int = 
  match (x, y) with
  | (a, 0) -> a
  | (a, b) -> gcd b (a mod b)

(* 5. Compute the McCarthy 91 function as shown in 
 * http://en.wikipedia.org/wiki/McCarthy_91_function
 *)
let rec mccarthy (n : int) : int = 
  if n > 100 then (n - 10) else mccarthy (mccarthy (n + 11))

(* 6. Compute the square root of x using Heron of Alexandria's
 * algorithm (circa 100 AD).  We start with an initial (poor)
 * approximate answer that the square root is 1.0 and then
 * continue improving the guess until we're within delta of the
 * real answer.  The improvement is achieved by averaging the
 * current guess with x/guess.  The answer is accurate to within
 * delta = 0.0001. *)
let squareRoot (x : float) : float =
  let delta = 0.0001 in
  let rec calc_guess guess = 
    let new_guess = (guess +. (x /. guess)) /. 2. in
    if Float.abs (new_guess -. guess) < delta 
    then new_guess else calc_guess new_guess
  in
  calc_guess 1.0
;;
