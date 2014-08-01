open Core.Std

(*** COS 326 Problem Set 1 ***)
let undefined : unit -> 'a = fun () -> failwith "undefined" ;;

(* 1. Please define these variables with the appropriate values.
 * Be sure that these statements all type-check after editing them.
 * You can do this by hitting Ctrl+c and then Ctrl+e in Emacs, or by
 * typing "make" in the terminal emulator *)

(* 1.a. Create a tuple with your first and last name *)
let name : (string * string) = ("Prakhar", "Srivastav")

(* 1.b. Create a string containing your email address *)
let email : string = "prakhar1989@gmail.com"

(* 1.c. Replace (Other "...") in class_year with the appropriate item below *)
(* ie: replace (Other "...") with Sophomore or Junior for example *)
type year = Freshman | Sophomore | Junior | Senior | Other of string;;

let class_year: year = Freshman

(* 1.d. Replace the .... with what you're excited about in this course *)
let exciting : string = "I'm excited about functional programming"

let print = Printf.printf;;

let print_survey = 
  let (first, last) = name in
  let string_year = 
    (match class_year with
       | Freshman -> "2016"
       | Sophomore -> "2015"
       | Junior -> "2014"
       | Senior -> "2013"
       | Other s -> "Other: " ^ s
    ) in
    (print "----------------------------------------\n";
     print "Name: %s %s\n" first last;
     print "Email: %s\n" email;
     print "Year: %s\n" string_year; 
     print "%s\n" exciting;
     print "----------------------------------------\n\n";);;

(* Problem 2 - Fill in types: 
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to remove the comments from each subproblem and to type check it
 * before submission. *)

(* Problem 2a *)
let prob2a : string  = 
  let greet y = "Hello " ^ y in greet "World!" ;;

(* Problem 2b *)
let prob2b : float option list = [Some 4.0; Some 2.7; None; Some 3.5] ;;

(*>* Problem 2c *>*)
let prob2c : ('a option * int option) * bool  = ((None, Some 42), true) ;;

(* Explain in a comment why each of 3a, 3b, 3c, 3d will not compile
 * and change the code in some small way so that it does. Do not
 * change the top-level type associated with the expression. *)

(*>* Problem 3a *>*)
(* Cannot compare float and int *)
let prob3a : bool = 
  let compare (x, y) = x < y in 
  compare (3.9, 4.) ;;

(*>* Problem 3b *>*)
(* nested tuples incorrectly shown in signature *)
let prob3b : string * (int * float)  = ("February", (1, 2.5)) 

(*>* Problem 3c *>*)
(* wrong matching on types and redundant pattern *)
let prob3c : int  = 
  match prob2c with 
  | (_, true) -> 1
  | (_, false) -> 2

(*>* Problem 3d *>*)
(* syntax errors *)
let prob3d : bool  = 
  match Some 3 with 
  | None -> true
  | Some _ -> false

(*>* Problem 4 *>*)
(* Fill in the ??? with an expression that uses w and has the right type *)
let prob2e =
  let v = (32.0, 28.0) in
  let square x = x *. x in
  let boff (w) = let (a, b) = w in square (a -. b) in
  let d = sqrt (boff v) in
  Int.of_float d
;;

(*>* Problem 5 *>*)

(* few_divisors n m should return true if n has fewer than m divisors, 
 * (including 1 and n) and false otherwise:
few_divisors 17 3;;
- : bool = false
# few_divisors 40 80;;
- : bool = true
# few_divisors 4 4;;
- : bool = false
 *) 
(* The type signature for few_divisors is: *)
(* few_divisors : int -> int -> bool *)

let few_divisors (n: int) (m: int): bool =
  let count_divisors x =
    let numberlist = List.init x ~f:(fun i -> i + 1) in
    let divisors = List.filter numberlist ~f:(fun i -> x mod i = 0) in
    List.length divisors
  in
  count_divisors n < count_divisors m
;;

(* Problem 6 - Approximating Pi *)

let bad_arg (s:string) = failwith ("bad argument: "^s);;

(*>* Problem 6a *>*)
(* Sinusoidal Approximation: write the function sin_pi *)
(* sin_pi : int -> float *)
(* use the following equations to define a function that returns the ith
 * approximation of pi.  Call bad_arg if the argument i to sin_pi is less
 * than 0.

 * approx(0) = 3
 * approx(n+1) = approx(n) + sin(approx(n))
 * approx(n) = approx(n-1) + sin(approx(n-1))
 *
 * approx(1) = 3 + sin(3)

 * Using this approximation, you will converge on many digits of pi very
 * fast.  The first few digits of pi are 3.14159 26535 89793 23846 26433.  
 * Approximation 1 accurately predicts these digits:  3.141
 * Approximation 2 accurately predicts these digits:  3.14159 26535
 * Approximation 3 accurately predicts these digits:  3.14159 26535 89793
 * 
 *)
let sin_pi (n: int) : float =
  let rec approx i =
    if i = 0 then 3. 
    else approx (i - 1) +. sin (approx (i - 1))
  in
  if n > 0 then approx n else bad_arg "arguments have to positive"
    
(*>* Problem 6b *>*)
(* Monte Carlo Approximation: write the function monte_pi
 *
 * monte_pi : int -> float
 *
 * A Monte Carlo method relies on repeated random sampling to simulate
 * some process or compute a value.  See Wikipedia:
 * http://en.wikipedia.org/wiki/Monte_Carlo_method
 * 
 * Pi can be computed using Monte Carlo simulation through a series
 * of experiments.  Here is a single experiment:
 *
 *  -- choose a pair of random floating point numbers between 0 and 1
 *  -- call the numbers x and y 
 *      -- think of (x,y) as a point on the plane in the unit square
 *  -- test whether the point falls within the unit circle by measuring
 *     the distance from the point to the origin:  x^2 + y^2 <= 1
 *
 * Now suppose you do m experiments and in n of those experiments, the
 * random point chosen falls within the upper right quarter of the unit circle.
 * Since the area of a circle is known to be pi * r^2 and the area of
 * a square is r^2 (and here we are dealing with a radius/square side
 * of length 1), the following equations hold:

  n    quarter of area of circle     1/4 * pi * r^2
 --- = -------------------------  =  -------------- = 1/4 * pi
  m        area of square                r^2

 * Use the above information to write the function monte_pi, which 
 * takes a positive number indicating the number of random points n to
 * sample and approximates pi using that number of random points.
 * Call bad_arg when a non-positive argument is thrown.
 *
 * To compute some random numbers, use O'Camls Random library:
 *
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html
 * 
 * We initialize the library below.
 *
 * Random.float f will return a random floating point number between 0.0 and f.
 *
 * Note: this estimation method will converge far more slowly than the
 * sinusoidal method (because the sin function already captures pi, so
 * that approximation was really cheating!).  I only had the first 2 
 * digits after 5000 trials.
 * I estimated pi at 3.141628 after 1,000,000 trials.
 *)

Random.init 17;; (* Don't remove this line; Your code follows *)

type point = { x: float; y: float }

let monti_pi (times: int): float = 
  let rec run (times:int) (within: int) = match times with
    | 0 -> within
    | t -> 
        let p = { x = Random.float 1.; y = Random.float 1. } in
        let distance {x = x; y = y} = sqrt (x *. x +. y *. y) in
        if distance p <= 1. 
        then (run (times - 1) (within + 1))
        else (run (times - 1) within)
  in
  let n = run times 0 in
  (Float.of_int n) *. 4. /. (Float.of_int times)
;;

(*************)
(* Problem 7 *)
(*************)

(* Look up another technique for approximating pi on the web.
 * As a starting point, see here:  
 *
 * http://en.wikipedia.org/wiki/Approximations_of_%CF%80
 *
 * You might be able to find other interesting articles on the web too.
 * 
 * The algorithm you choose must be capable of computing many digits of
 * pi.  Algorithms that compute just 1 approximation (such as 3 or
 * 3927/1250 or any other fixed fraction) are insufficient.  Choose 
 * an algorithm that successively approximates pi in some manner.  
 * Your algorithm may not use trigonometric functions such as sin, 
 * cos, arctan, etc.
 *
 *)

(* 7a:  Explain your algorithm and your sources here:




 *)

(* 7b:  Implement your algorithm here. *)
(*      Your algorithm may take a parameter that indicates the number of
 *      rounds used in the approximation, or some other information.
 *      Explain what the parameter is and show some tests.
 *)
