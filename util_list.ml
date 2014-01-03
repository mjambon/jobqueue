let unique l =
  BatList.unique l

(* diff [1;2;3;4] [9;3;5;1] = [2;4] *)
let diff l1 l2 =
  List.filter (fun k -> not (List.mem k l2)) (unique l1)

let union l1 l2 = unique (l1 @ l2)
