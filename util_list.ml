(*
   Various utilities for handling lists
*)

let sort_full ?(compare = compare) get_key l =
  let kv_list = List.rev_map (fun v -> (get_key v, v)) l in
  let kv_list = List.sort (fun (k1, v1) (k2, v2) -> compare k2 k1) kv_list in
  List.rev_map snd kv_list

let test_sort_full () =
  sort_full String.lowercase ["C"; "A"; "b"] = ["A"; "b"; "C"]

(*
   Remove from the first all duplicate elements based on their keys
   as well as elements whose keys appear in the second list,
   proceeding from left to right.
*)
let diff_full get_key1 get_key2 l1 l2 =
  let tbl = Hashtbl.create (2 * List.length l1) in
  List.iter (fun x ->
    let k = get_key2 x in
    Hashtbl.replace tbl k ()
  ) l2;
  let r =
    List.fold_left (fun acc x ->
      let k = get_key1 x in
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
  diff_full (fun x -> x) (fun x -> x) l1 l2

(* Remove duplicate elements,
   proceeding from left to right unlike BatList.unique *)
let unique_full get_key l =
  diff_full get_key get_key l []

let unique l =
  unique_full (fun x -> x) l

let test_unique () =
  let input = [3;2;5;1;2;3;8;4;8;2] in
  let expected_output = [3;2;5;1;8;4] in
  unique input = expected_output

(*
   Remove duplicates from the union of the lists, proceeding
   from left to right:
     union_full (fun x -> x) [1; 2; 3] [4; 3; 5] = [1; 2; 3; 4; 5]
*)
let union_full get_key l1 l2 = unique_full get_key (l1 @ l2)

let union l1 l2 = union_full (fun x -> x) l1 l2

(*
   Return a list of elements from the first list whose keys
   exist in the second list as well.
   The result does not contain duplicates.
*)
let inter_full get_key1 get_key2 l1 l2 =
  let tbl = Hashtbl.create (2 * List.length l2) in
  List.iter (fun x ->
    let k = get_key2 x in
    Hashtbl.replace tbl k x
  ) l2;
  List.filter (fun x ->
    let k = get_key1 x in
    let b = Hashtbl.mem tbl k in
    if b then
      Hashtbl.remove tbl k;
    b
  ) l1

let inter l1 l2 =
  let l1, l2 =
    (* optimize by building a hash table from the shortest list *)
    if List.length l1 <= List.length l2 then
      l1, l2
    else
      l2, l1
  in
  inter_full
    (fun x -> x)
    (fun x -> x)
    l1 l2

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

(*
   Find the minimum of a non-empty list according to the given
   comparison function `cmp` (e.g. Pervasives.compare).
   `list_first cmp l` is equivalent to `List.hd (List.stable_sort cmp l)`
   but costs only O(length l).
*)
let get_first cmp l =
  match l with
  | [] -> assert false
  | first :: tail ->
      List.fold_left (fun acc x ->
        if cmp acc x <= 0 then acc
        else x
      ) first tail

let test_get_first () =
  assert (get_first compare [3;2;1;5;4] = 1);
  assert (get_first (fun a b -> compare b a) [3;2;1;5;4] = 5);
  true

(*
   Get an element with the most common property specified as the key k
   in each pair (k, v). A sample value is returned with the key.
*)
let get_majority pair_list =
  let clusters = group_by_key pair_list in
  let counts =
    BatList.map (fun (k, vl) ->
      match vl with
      | [] -> assert false
      | first :: _ -> (List.length vl, (k, first))
    ) clusters
  in
  match clusters with
  | [] ->
      invalid_arg "Util_list.get_majority"
  | l ->
      snd (get_first (fun (n1, _) (n2, _) -> compare n2 n1) counts)

let test_get_majority () =
  let k, v = get_majority [ 1, "a";
                            2, "b";
                            2, "c";
                            2, "d";
                            3, "e";
                            3, "f" ]
  in
  assert (k = 2);
  assert (
    match v with
      | "b" | "c" | "d" -> true
      | _ -> false
  );
  true

(*
   If possible, get a pair (key, value) whose key matches Some _
   and is the most frequent in the input list, otherwise return
   a pair whose key is None.
*)
let get_opt_majority pair_list =
  let nones, somes = BatList.partition (fun (o, v) -> o = None) pair_list in
  match somes with
  | [] -> get_majority nones
  | _ -> get_majority somes

let test_get_opt_majority () =
  let k, v = get_opt_majority [ Some 1, "a";
                                None, "b";
                                None, "c";
                                None, "d";
                                Some 3, "e";
                                Some 3, "f";
                                Some 4, "g" ]
  in
  assert (k = Some 3);
  assert (
    match v with
      | "e" | "f" -> true
      | _ -> false
  );

  let k, v = get_opt_majority [ None, "a"; None, "b" ] in
  assert (k = None);

  true

(*
   Put a list of items into a hash table, removing duplicates.
*)
let to_table l get_key =
  let tbl = Hashtbl.create (List.length l) in
  List.iter (fun x -> Hashtbl.replace tbl (get_key x) x) l;
  tbl

(*
   Common functions with arguments in a better order,
   and which won't blow the stack
*)
let iter l f = BatList.iter f l
let map l f = BatList.map f l
let fold_left acc l f = BatList.fold_left f acc l
let fold_right l acc f = BatList.fold_right f l acc
let filter l f = BatList.filter f l
let filter_map l f = BatList.filter_map f l
let for_all l f = BatList.for_all f l
let exists l f = BatList.exists f l
let find l f = BatList.find f l


let tests = [
  "get_first", test_get_first;
  "get_majority", test_get_majority;
  "get_opt_majority", test_get_opt_majority;
  "sort", test_sort_full;
  "unique", test_unique;
  "inter", test_inter;
  "group by key", test_group_by_key;
]
