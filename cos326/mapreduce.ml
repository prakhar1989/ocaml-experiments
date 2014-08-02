open Core.Std

let added_list = List.map ~f:(fun (x, y) -> x + y) [(1, 3); (4, 2); (3, 0)]
