(*
   Simple implementation of a temporary cache.

   It works with lwt threads too, so we can do:

     let get_account = memoize User_account.get_account

     (* first call fetches the account *)
     get_account uid >>= fun account ->

     (* second call gets it from the cache *)
     get_account uid >>= fun account ->
     ...
*)

(* Keys must be comparable and hashable with the default functions *)
let memoize get_key f =
  let tbl = Hashtbl.create 20 in
  fun x ->
    let key = get_key x in
    try Hashtbl.find tbl key
    with Not_found ->
      let t = f x in
      Hashtbl.add tbl key t;
      t
