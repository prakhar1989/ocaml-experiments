let rec zip (xs: int list) (ys: int list) : (int * int ) list option =
  match (xs, ys) with
  | ([], []) -> Some []
  | (x :: xs', y :: ys') -> 
    (match zip xs' ys' with 
    | None -> None
    | Some zs -> Some ((x, y) :: zs))
  | (_, _) -> None
;;

(** Insertion Sort *)
(* insert x in a sorted list *)
let rec insert (x: int) (xs: int list) : int list = 
  match xs with 
  | [] -> []
  | hd :: tl -> if x < hd then x :: xs else hd :: insert x tl

(* insertion sort *)
let insert_sort (xs: int list): int list =
  let rec aux (sorted: int list) (unsorted: int list): int list = 
    match unsorted with
    | [] -> sorted
    | hd :: tl -> aux (insert hd sorted) tl
  in
  aux [] xs
