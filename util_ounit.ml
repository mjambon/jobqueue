open Printf

let test description f =
  let open OUnit in
  description >:: (fun () ->
    let b =
      try f ()
      with e ->
        eprintf "Uncaught exception: %s\n" (Util_exn.string_of_exn e);
        false
    in
    assert_bool "failed" b
  )

let tests module_ l =
  let open OUnit in
  module_ >::: List.map (fun (desc, f) -> test desc f) l
