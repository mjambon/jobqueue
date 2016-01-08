(*
   Simple hashtable-based counters
*)

let create () = Hashtbl.create 100

let incr tbl k =
  let r =
    try Hashtbl.find tbl k
    with Not_found ->
      let r = ref 0 in
      Hashtbl.add tbl k r;
      r
  in
  incr r

(*
   Return all counters in decreasing order
*)
let get tbl =
  let l = Hashtbl.fold (fun k r acc -> (k, !r) :: acc) tbl [] in
  let a = Array.of_list l in
  Array.sort (fun (x1, (n1 : int)) (x2, n2) ->
    let c = compare n2 n1 in
    if c <> 0 then c
    else
      compare x1 x2
  ) a;
  Array.to_list a
