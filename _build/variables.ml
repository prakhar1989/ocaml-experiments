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

(* helper *)
let print_int_list list = 
  List.iter list (printf "%d ") ^ "\n"

(* main function *)
let () = print_int_list incs
