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

      foo is initialized in this module. The value it takes is typically
      a function
   *)
   let foo x y =
     ...

   let () = init_foo foo    (* <-- initialization *)
*)

let create name =
  let r = ref None in
  let init f =
    match !r with
    | None -> r := Some f
    | Some _ -> failwith (name ^ " was already initialized")
  in
  let call x =
    match !r with
    | None -> failwith ("Uninitialized function " ^ name)
    | Some f -> f x
  in
  init, call

let test_late_initialization () =
  let init_foo, foo = create "foo" in
  (try
     ignore (foo 1 2);
     assert false
   with _ -> ());

  let real_foo x y = x + y in
  init_foo real_foo;
  (try
     init_foo real_foo;
     assert false
   with _ -> ());

  assert (foo 3 4 = 7);
  true

let tests = [
  "late initialization", test_late_initialization;
]
