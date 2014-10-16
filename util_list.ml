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

(*
   Return a list of elements that exist in both lists.
   The result does not contain duplicates.
*)
let inter_full get_key l1 l2 =
  let short, long =
    let len1 = List.length l1
    and len2 = List.length l2 in
    if len1 < len2 then
      l1, l2
    else
      l2, l1
  in
  let tbl = Hashtbl.create (2 * List.length short) in
  List.iter (fun x ->
    let k = get_key x in
    Hashtbl.replace tbl k x
  ) short;
  List.filter (fun x ->
    let k = get_key x in
    let b = Hashtbl.mem tbl k in
    if b then
      Hashtbl.remove tbl k;
    b
  ) long

let inter l1 l2 = inter_full (fun x -> x) l1 l2

let test_inter () =
  let result = inter [1;2;3;4;3] [5;4;6;1;4;3] in
  let expected = [1;3;4] in
  List.sort compare result = List.sort compare expected

let group_by_key pair_list =
  let tbl = Hashtbl.create (List.length pair_list) in
  List.iter (fun (k, v) ->
    let r =
      try Hashtbl.find tbl k
      with Not_found ->
        let r = ref [] in
        Hashtbl.add tbl k r;
        r
    in
    r := v :: !r
  ) pair_list;
  Hashtbl.fold (fun k r acc -> (k, List.rev !r) :: acc) tbl []

let test_group_by_key () =
  List.sort compare (group_by_key [1,2;
                                   2,4;
                                   1,5;
                                   3,6;
                                   3,7])
  = List.sort compare [1, [2; 5];
                       2, [4];
                       3, [6; 7]]

let tests = [
  "unique", test_unique;
  "inter", test_inter;
  "group by key", test_group_by_key;
]
