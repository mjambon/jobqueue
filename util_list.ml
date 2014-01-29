(* Remove duplicate elements and elements from the second list,
   proceeding from left to right. *)
let diff_full get_key l1 l2 =
  let tbl = Hashtbl.create (2 * List.length l1) in
  List.iter (fun x ->
    let k = get_key x in
    Hashtbl.replace tbl k ()
  ) l2;
  let r =
    List.fold_left (fun acc x ->
      let k = get_key x in
      if Hashtbl.mem tbl k then acc
      else (
        Hashtbl.add tbl k ();
        x :: acc
      )
    ) [] l1
  in
  List.rev r

(* diff [1;2;3;4] [9;3;5;1] = [2;4] *)
let diff l1 l2 =
  diff_full (fun x -> x) l1 l2

(* Remove duplicate elements,
   proceeding from left to right unlike BatList.unique *)
let unique_full get_key l =
  diff_full get_key l []

let unique l =
  unique_full (fun x -> x) l

let test_unique () =
  let input = [3;2;5;1;2;3;8;4;8;2] in
  let expected_output = [3;2;5;1;8;4] in
  unique input = expected_output

let union_full get_key l1 l2 = unique_full get_key (l1 @ l2)

let union l1 l2 = union_full (fun x -> x) l1 l2

let tests = [
  "unique", test_unique;
]
