
(*
   Create an unitialized single-value container, return getter and setter.
   The setter may be applied at most once.
   The getter may only be called after a value was set.
   initializer_name is for error messages.

   That's somewhat equivalent to a Java 'final'. That means don't use it,
   especially OCaml beginners.
*)
let make_initializer name =
  let x = ref None in
  let get () =
    match !x with
    | None -> failwith ("Uninitialized variable " ^ name)
    | Some taskid -> taskid
  and set_once taskid =
    match !x with
    | Some _ -> failwith ("Variable is already initialized: " ^ name)
    | None -> x := Some taskid
  in
  get, set_once
