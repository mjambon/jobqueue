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
   Duplicates are removed.
   The result is the first list from which elements have been removed,
   i.e. the order of the first list is preserved.
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

(*
   Sort a list of values according to some order specified by a list of
   keys.
   Values whose key doesn't appear in the list of keys are moved to the end.
*)
let reorder get_key keys values =
  let tbl = Hashtbl.create (2 * List.length keys) in
  BatList.iteri (fun i k ->
    if not (Hashtbl.mem tbl k) then
      Hashtbl.add tbl k i
  ) keys;
  let default = List.length keys in
  let l =
    BatList.map (fun v ->
      try (Hashtbl.find tbl (get_key v), v)
      with Not_found -> (default, v)
    ) values
  in
  let l = BatList.stable_sort (fun (i, _) (j, _) -> compare i j) l in
  BatList.map snd l

let test_reorder () =
  let f keys values = reorder (fun k -> k) keys values in
  (match f [4;2;6;3] [1;2;3;4;5;6;7] with
   | 4 :: 2 :: 6 :: 3 :: tail ->
       assert (List.sort compare tail = [1; 5; 7])
   | _ ->
       assert false
  );
  (match f [6;4;2;6;3] [1;1;2;3;4;5;6;7] with
   | 6 :: 6 :: 4 :: 2 :: 3 :: tail ->
       assert (List.sort compare tail = [1; 1; 5; 7])
   | _ ->
       assert false
  );
  true

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
   Group elements with an identical key,
   which must be hashable and comparable (usable as key with Hashtbl).
   The `get_representative` function must return a key and a value
   that is usable as the representative of the cluster.
*)
let group_by get_representative l =
  let pair_list =
    List.rev_map (fun x ->
      let k, v = get_representative x in
      (k, (v, x))
    ) l
  in
  let groups = group_by_key pair_list in
  BatList.map (fun (k, l) ->
    match l with
    | [] -> assert false
    | (v, x) :: _ -> (v, BatList.map snd l)
  ) groups

let test_group_by () =
  let get_representative (k, v) = (k, 10 + v) in
  let l =
    group_by get_representative
      [1,2;
       1,4]
  in
  match l with
  | [ (12|14), ([1,2; 1,4]|[1,4; 1,2]) ] -> true
  | _ -> false

(*
   Split list into uniques and the rest.
*)
let split_unique_full get_key l =
  let tbl = Hashtbl.create (2 * List.length l) in
  BatList.partition (fun x ->
    let k = get_key x in
    if Hashtbl.mem tbl k then false
    else (
      Hashtbl.add tbl k ();
      true
    )
  ) l

let unique_first_full get_key l =
  let uniques, other = split_unique_full get_key l in
  uniques @ other

let unique_first l =
  unique_first_full (fun x -> x) l

let test_unique_first () =
  let f l = unique_first_full floor l in
  assert (f [] = []);
  assert (f [123.] = [123.]);
  assert (f [1.1; 1.; 2.] = [1.1; 2.; 1.]);
  assert (f [5.; 5.1; 2.; 3.; 3.1; 2.1; 4.] = [5.; 2.; 3.; 4.; 5.1; 3.1; 2.1]);
  true

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
let make_kv_table l get_kv =
  let tbl = Hashtbl.create (List.length l) in
  List.iter (fun x ->
    let k, v = get_kv x in
    Hashtbl.replace tbl k v
  ) l;
  tbl

(* Simpler interface to `make_kv_table` *)
let to_kv_table pairs =
  make_kv_table pairs (fun x -> x)

(* Simpler interface to `make_kv_table` *)
let to_table l get_key =
  make_kv_table l (fun x -> (get_key x, x))

(* Simpler interface to `to_kv_table`, storing no value.
   Meant to be used with `Hashtbl.mem` for efficient existence. *)
let to_mem_table l =
  make_kv_table l (fun k -> (k, ()))

let optimum l prefer_right_arg =
  match l with
  | [] -> invalid_arg "Util_list.optimum"
  | first :: rest ->
      List.fold_left (fun acc x ->
        if prefer_right_arg acc x then x
        else acc
      ) first rest

(* Find the maximum element of a list, preferring the leftmost occurrence. *)
let maximum l cmp =
  optimum l (fun a b -> cmp a b < 0)

(* Find the minimum element of a list, preferring the leftmost occurrence. *)
let minimum l cmp =
  optimum l (fun a b -> cmp a b > 0)

let test_optimum () =
  assert (
    maximum [0.; 1.1; 1.2; 0.] (fun a b -> compare (truncate a) (truncate b))
    = 1.1
  );
  assert (
    minimum [1.; 0.2; 0.1; 1.] (fun a b -> compare (truncate a) (truncate b))
    = 0.2
  );
  true

(*
   Return true if a predicate matches at least n elements of the list.
   `List.exists f lst` is equivalent to `List.exists_n f 1 lst`.
*)
let rec exists_n n l f =
  if n <= 0 then
    true
  else
    match l with
    | [] ->
        false
    | x :: l ->
        let n =
          if f x then (n-1)
          else n
        in
        exists_n n l f

let test_exists_n () =
  let f x = x = 0 in
  assert (exists_n 3 [1;2;3;0;1;0;1;0;0] f);
  assert (exists_n 3 [0;0;0] f);
  assert (not (exists_n 1 [] f));
  assert (not (exists_n 1 [5] f));
  assert (exists_n 0 [5] f);
  assert (exists_n 0 [] f);
  true

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
  "unique first", test_unique_first;
  "get_first", test_get_first;
  "get_majority", test_get_majority;
  "get_opt_majority", test_get_opt_majority;
  "sort", test_sort_full;
  "unique", test_unique;
  "inter", test_inter;
  "reorder", test_reorder;
  "group by key", test_group_by_key;
  "group by", test_group_by;
  "optimum", test_optimum;
  "exists_n", test_exists_n;
]
