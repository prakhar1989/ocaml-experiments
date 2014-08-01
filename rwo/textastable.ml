open Core.Std

(* type definitions *)
type header = string list
type rows = string list list

(* implementation *)
let max_length header rows : (int list) = 
  let lengths list = List.map ~f:String.length list in
  List.fold rows
    ~init: (lengths header)
    ~f: (fun acc row ->
      List.map2_exn ~f:Int.max acc (lengths row))

let make_separator (widths: int list) : (string) = 
  let pieces = List.map widths 
    ~f:(fun w -> String.make (w + 2) '-') (* padding of 2 chars *)
  in
  "+" ^ String.concat ~sep:"+" pieces ^ "+"

let pad (s: string) (width: int) : (string) = 
  " " ^ s ^ String.make (width - String.length s + 1) ' '

let render_row (row: string list) (widths: int list) : (string) =
  let items = List.map2_exn row widths ~f:pad in 
  "|" ^ String.concat items ~sep:"|" ^ "|"

let render_table header rows : (string) =
  let widths = max_length header rows in
  let sep = make_separator widths in
  String.concat ~sep:"\n" 
    ((sep :: render_row header widths :: 
      sep :: List.map rows ~f:(fun x -> render_row x widths)) @ [sep])

(* main *)
let () =
  let header = ["Language";"Architect";"First Release"] in
  let rows = [ 
     ["Lisp"    ; "John McCarthy"    ; "1958"] ;
     ["Scala"   ; "Martin Odersky"   ; "2003"] ;
     ["ML"      ; "Robin Milner"     ; "1973"] ;
     ["OCaml"   ; "Xavier Leroy"     ; "1996"] ;
  ] in
  print_endline (render_table header rows)
