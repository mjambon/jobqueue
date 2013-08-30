let assert_string_equal =
  OUnit.assert_equal ~printer:(fun s -> Printf.sprintf "'%s'" s)

let assert_int_equal =
  OUnit.assert_equal ~printer:string_of_int

let assert_float_equal =
  OUnit.assert_equal ~printer:string_of_float

let assert_bool_equal =
  OUnit.assert_equal ~printer:string_of_bool

let test description f =
  let open OUnit in
  description >:: (fun () -> assert_bool "failed" (f ()))

let tests module_ l =
  let open OUnit in
  module_ >::: List.map (fun (desc, f) -> test desc f) l
