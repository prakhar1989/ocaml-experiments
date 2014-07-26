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
  "+" ^ String.concat ~sep:"+" pieces ^ "+"

let pad string width = 
  " " ^ string ^ String.make (width - String.length string + 1) ' '

let render_row row widths =
  let items = List.map2_exn row widths ~f:pad in 
  "|" ^ String.concat items ~sep:"|" ^ "|"

let render_table header rows =
  let widths = max_length header rows in
  let sep = make_separator widths in
  String.concat ~sep:"\n" 
    ((sep :: render_row header widths :: 
      sep :: List.map rows ~f:(fun x -> render_row x widths)) @ [sep])

(* filter map removes all values that return a None *)
let filenames = 
  List.filter_map (Sys.ls_dir ".") 
    ~f: (fun filename ->
      match String.rsplit2 ~on:'.' filename with
      | None -> None
      | Some ("", _) -> None
      | Some (_, ext) -> Some ext)
    |> List.dedup

let is_ocaml_source string = 
  match String.rsplit2 string ~on:'.' with
  | Some (_, ("ml"|"mli")) -> true
  | _ -> false

let (ml_files, other_files) =
  List.partition_tf (Sys.ls_dir ".") ~f:is_ocaml_source

(* tail call recursion *)
let rec length_plus_n list n = 
  match list with
  | [] -> n
  | _ :: tl -> length_plus_n tl (n + 1)

let rec destutter = function
  | [] | [_] as l -> l
  | hd :: (hd' :: _ as tl) when hd = hd' -> destutter tl
  | hd :: tl -> hd :: destutter tl

let () =
  let header = ["Language";"Architect";"First Release"] in
  let rows = [ 
     ["Lisp"    ; "John McCarthy"    ; "1958"] ;
     ["Scala"   ; "Martin Odersky"   ; "2003"] ;
     ["OCaml"   ; "Xavier Leroy"     ; "1996"] ;
  ] in
  print_endline (render_table header rows)
