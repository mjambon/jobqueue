let test description f =
  let open OUnit in
  description >:: (fun () -> assert_bool "failed" (f ()))

let tests module_ l =
  let open OUnit in
  module_ >::: List.map (fun (desc, f) -> test desc f) l
