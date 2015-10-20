(*
   One-time initialization.

   Useful to create mutual dependencies between modules.

   Usage:

   (* Module A *)

   let init_foo, foo = create "foo"

   let f () =
     ...
     foo x y       (* <-- must take place after initialization *)
     ...

   (* Module B

      Function foo is initialized in this module.
   *)
   let foo x y =
     ...

   let () = A.init_foo foo    (* <-- initialization *)
*)

let uninitialized = Hashtbl.create 10

let create_id =
  let n = ref 0 in
  fun () ->
    let id = !n in
    assert (id >= 0);
    incr n;
    id

let create name =
  let r = ref None in
  let id = create_id () in
  Hashtbl.add uninitialized id name;
  let init f =
    match !r with
    | None ->
        Hashtbl.remove uninitialized id;
        r := Some f
    | Some _ ->
        failwith (name ^ " was already initialized")
  in
  let call x =
    match !r with
    | None ->
        failwith ("Uninitialized function " ^ name)
    | Some f ->
        f x
  in
  init, call

(*
   Return the list of uninitialized functions, sorted by creation date.
*)
let get_all_uninitialized () =
  let l =
    Hashtbl.fold (fun id name acc -> (id, name) :: acc) uninitialized []
  in
  let sorted = List.sort (fun (a, _) (b, _) -> compare a b) l in
  List.map snd sorted

(*
   Print an error and exit the process if anything is uninitialized.
*)
let check_all () =
  match get_all_uninitialized () with
  | [] -> ()
  | l ->
      Printf.eprintf
        "The following functions were not initialized: %s\n%!"
        (String.concat ", " l);
      exit 1

let test_late_initialization () =
  let n0 = List.length (get_all_uninitialized ()) in
  let init_foo, foo = create "foo" in
  assert (List.length (get_all_uninitialized ()) = n0 + 1);
  (try
     ignore (foo 1 2);
     assert false
   with _ -> ());

  let real_foo x y = x + y in
  init_foo real_foo;
  assert (List.length (get_all_uninitialized ()) = n0);
  (try
     init_foo real_foo;
     assert false
   with _ -> ());

  assert (foo 3 4 = 7);
  true

let tests = [
  "late initialization", test_late_initialization;
]
