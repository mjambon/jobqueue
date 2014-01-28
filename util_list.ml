(* Remove duplicate elements proceeding from left to right,
   unlike BatList.unique *)
let unique l =
  let tbl = Hashtbl.create (2 * List.length l) in
  let r =
    List.fold_left (fun acc x ->
      if Hashtbl.mem tbl x then acc
      else (
        Hashtbl.add tbl x ();
        x :: acc
      )
    ) [] l
  in
  List.rev r

let test_unique () =
  let input = [3;2;5;1;2;3;8;4;8;2] in
  let expected_output = [3;2;5;1;8;4] in
  unique input = expected_output

(* diff [1;2;3;4] [9;3;5;1] = [2;4] *)
let diff l1 l2 =
  List.filter (fun k -> not (List.mem k l2)) (unique l1)

let union l1 l2 = unique (l1 @ l2)

let tests = [
  "unique", test_unique;
]
