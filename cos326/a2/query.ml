(* Box office analysis *)

(* Contents:
    -- the movie type
    -- the studio_gross type
    -- functions for querying and transforming lists of movies
*)

(* a movie is a tuple of (title, studio, gross in millions, year) *)
type movie = string * string * float * int;;

(* a studio_gross is a pair of (studio, gross in millions) *)
type studio_gross = string * float;;

(* call bad_argument if your function receives a bad argument *)
(* do not change this exception or function                   *)
exception Bad_arg of string
let bad_arg (s:string) = raise (Bad_arg s)

(* a useful debugging routine *)
let debug s = print_string s; flush_all()

(* *** DO NOT CHANGE DEFINITIONS ABOVE THIS LINE! *** *)

(* A simple reduce function *)
let rec reduce (xs: 'a list) (f: 'a -> 'b -> 'b) (init: 'b): 'b = 
  match xs with
  | [] -> init
  | hd :: tl -> reduce tl f (f hd init)
;;

(* A simple map function *)
let rec map (xs: 'a list) (f: 'a -> 'b): 'b list = 
  match xs with
  | [] -> []
  | hd :: tl -> (f hd) :: (map tl f)
;;

(* A simple filter function *)
let rec filter (xs: 'a list) (f: 'a -> bool): 'a list = 
  match xs with
  | [] -> []
  | hd :: tl -> 
      if f hd then hd :: filter tl f
      else filter tl f
;;

(* Getter functions  - Should movie be a record? *)
let get_movie_gross (m: movie): float = 
  let _, _, gross, _ = m in
  gross
;;

let get_movie_year (m: movie): int = 
  let _, _, _, year = m in 
  year
;;

let length (xs: 'a list): int = reduce xs (fun _ acc -> acc + 1) 0;;

(* find the average gross of the movies in the list                  *)
(* hint: you may need to use functions float_of_int and int_of_float *)
(* hint: if you don't know what those functions do,                  *)
(*       type them in to ocaml toplevel                              *)
(* hint: recall the difference between +. and + also 0. and 0        *)
let average (movies : movie list) : float = 
  let total = reduce movies (fun mov accum -> accum +. get_movie_gross mov) 0. in
  let len = float_of_int (length movies) in
  total /. len
;;

(* return a list containing only the movies from the given decade *)
(* call bad_arg if n is not 20, 30, ..., 90, 00, 10               *)
let decade (n:int) (ms:movie list) : movie list =  
  if not (n mod 10 = 0)
  then bad_arg "Not a valid decade"
  else 
    let year_in_decade dec year : bool = 
      let decade = dec + if dec < 20 then 2000 else 1900 in
      year - decade >= 0 && year - decade < 10
    in
    filter ms (fun mov -> year_in_decade n (get_movie_year mov))
;;

(* return the first n items from the list *)
let rec take (n:int) (l:'a list)  : 'a list =
  match l with 
  | [] -> []
  | hd :: tl -> 
      if n = 0 then [] 
      else hd :: take (n - 1) tl
;;

(* return everything but the first n items from the list *)
let rec drop (n:int) (l:'a list)  : 'a list =
  match l with
  | [] -> []
  | hd :: tl -> 
      if n = 0 then hd :: drop n tl
      else drop (n - 1) tl
;;

(* return a list [x1; x2; ...; xn] with the same elements as the input l
   and where:
     leq xn xn-1
     ...
     leq x3 x2
     leq x2 x1
     are all true
*)
(* hint: define an auxiliary function "select" *)
(* hint: select probably should return a pair.  Of what?  Think functionally. *)
type 'a less = 'a -> 'a -> bool;;
let rec selection_sort (leq:'a less) (l:'a list) : 'a list =
  failwith "selection_sort unimplemented"
;;
 
(* return list of movies sorted by gross (largest gross first) *)
let sort_by_gross (movies : movie list) : movie list = 
  failwith "sort_by_gross unimplemented"
;;

(* return list of movies sorted by year produced (largest year first) *)
let sort_by_year (movies : movie list) : movie list = 
  failwith "sort_by_year unimplemented"
;;

(* sort list of (studio, gross in millions) by gross in millions 
 * with the largest gross first *)
let sort_by_studio (studio_grosses : studio_gross list) : studio_gross list = 
  failwith "sort_by_studio unimplemented"
;;

(* given list of movies,
 * return list of pairs (studio_name, total gross revenue for that studio)  *)
let by_studio (movies:movie list) : studio_gross list =
  failwith "by_studio unimplemented"
;;

(*********************************)
(* Karma -- Optional Questions!! *)
(*********************************)

(* return the list of all permutations of input list *)
(* test this on small inputs!! *)
let perm (items:'a list) : 'a list list =
  failwith "perm unimplemented"
;;

(* randomize the order of the items in the list *)
(* explain your method in a comment *)
(* extra karma: do it in time less than N^2 where N is the length of the list *)
let scramble (items:'a list) : 'a list =
  failwith "scramble unimplemented"
;;

(***********)
(* Testing *)
(***********)

(* Augment the testing infrastructure below as you see fit *)

(* Test Data *)

let data1 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003)
];;

let data2 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012)
];;

let data3 : movie list = [
  ("Harry Potter and the Sorcerer's Stone","WB",317.57555,2001);
  ("Star Wars: Episode II - Attack of the Clones","Fox",310.67674,2002);
  ("Return of the Jedi", "Fox", 309.306177, 1983)
];;

let data4 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012);
  ("The Dark Knight","WB",533.34,2008);
  ("Harry Potter and the Deathly Hallows Part 2","WB",381.01,2011)
];;

let data5 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012);
  ("The Dark Knight","WB",533.34,2008);
  ("Harry Potter and the Deathly Hallows Part 2","WB",381.01,2011);
  ("Scary 2","WB",381.01,1989);
  ("Scary 1","WB",134.12,1985);
];;

(* Assertion Testing *)

let get_average (xs: float list): float = 
  let len = length xs in
  let total = reduce xs (fun x y -> x +. y) 0. in
  total /. (float_of_int len)
;;


(* Uncomment the following when you are ready to test your take routine *)
assert(take 0 data4 = []);;
assert(take 1 data1 = data1);;
assert(take 2 data4 = data2);;
assert(take 5 data2 = data2);;
assert(take 2 data2 = data2);;

assert(average data1 = get_average [377.85]);;
assert(average data2 = get_average [377.85; 374.32]);;
assert(average data3 = get_average [317.57555; 310.67674; 309.306177]);;
assert(average data4 = get_average [377.85; 374.32; 533.34; 381.01]);;

assert(length (decade 10 data5) = 2);;
assert(length (decade 00 data5) = 2);;
assert(length (decade 80 data5) = 2);;
assert(length (decade 90 data5) = 0);;

(* Additional Testing Infrastructure *)

let stests : (unit -> movie list) list = [
  (fun () -> sort_by_gross data1);
  (fun () -> sort_by_gross data2);
  (fun () -> sort_by_gross data3);
  (fun () -> sort_by_gross data4)
];;

let check (i:int) (tests:(unit -> 'a) list) : 'a =
  if i < List.length tests && i >= 0 then
    List.nth tests i ()
  else
    failwith ("bad test" ^ string_of_int i)
;;

(* debugging stuff 
debug (string_of_float (average data4))
*)
