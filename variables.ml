open Core.Std

let languages = "OCaml,Perl,C,C++"
let dashed_languages = 
  let language_list = String.split languages ~on:',' in
  String.concat ~sep:"-" language_list

(* pattern matching with let *)
let (ints, strings) = List.unzip [(1, "one"); (2, "two"); (3, "three")]
let squares = List.map ints ~f:(fun x -> x * x)

let upcase_first_entry line = 
  match String.split line ~on:',' with
  | [] -> assert false (* case impossible - string split returns at least one *)
  | first :: rest -> String.concat ~sep:"," (String.uppercase first :: rest)

(* functions in lists *)
let increments = [ (fun x -> x + 1); (fun x -> x + 2) ]
let incs = List.map increments ~f:(fun g -> g 5)

(* currying *)
let abs_diff x y = abs (x - y)
let distance_from_three = abs_diff 3

(* recursion *)
let rec find_first_stutter list = 
  match list with
  | [] -> None
  | [_] -> None
  | x :: y :: tl ->
      if x = y then Some x 
      else find_first_stutter (y :: tl)

(* operators *)
let (+!) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
let (x,y) = (3,4) +! (6,8)

(* |> operator which does x f = f x  - awesome for chaining *)
let path = "/usr/bin:/usr/local/bin:/bin:/sbin:/bin"
let uniq_paths = 
  String.split ~on:':' path
  |> List.dedup ~compare:String.compare

(* helper *)
let print_int_list list = 
  List.iter list (printf "%d ")

let () = 
  print_int_list incs

let () =
  List.iter ~f:print_endline uniq_paths
