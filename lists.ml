open Core.Std

let ints = [1;2;3]
let more_ints = 4 :: 5 :: 6 :: []

let rec sum list = 
  match list with 
  | [] -> 0
  | hd :: tl -> hd + sum tl

let rec drop_value list value =
  match list with
  | [] -> []
  | hd :: tl -> 
    let new_tl = drop_value tl value in
    if hd = value then new_tl else hd :: new_tl

(*
| language | architect      | first release |
|----------+----------------+---------------|
| Lisp     | John McCarthy  | 1958          |
| C        | Dennis Ritchie | 1969          |
| ML       | Robin Milner   | 1973          |
| OCaml    | Xavier Leroy   | 1996          |
*)

let max_length header rows = 
  let lengths list = List.map ~f:String.length list in
  List.fold rows
    ~init: (lengths header)
    ~f: (fun acc row ->
      List.map2_exn ~f:Int.max acc (lengths row))

let make_separator widths = 
  let pieces = List.map widths 
    ~f:(fun w -> String.make (w + 2) '-') (* padding of 2 chars *)
  in
  "|" ^ String.concat ~sep:"+" pieces ^ "|"

let () =
  let header = ["language";"architect";"first release"] in
  let rows = [ 
     ["Lisp" ;"John McCarthy" ;"1958"];
     ["C"    ;"Dennis Ritchie";"1969"];
     ["ML"   ;"Robin Milner"  ;"1973"];
     ["OCaml";"Xavier Leroy"  ;"1996"];
  ] in
  let widths = max_length header rows in
  print_endline (make_separator widths)
