(* See .mli *)

type t = unit -> unit

let create () =
  fun () -> ()

type test = {
  first: t;
  other: int;
}

let make_test_value x =
  { first = create ();
    other = x }

let test_comparison () =
  try
    ignore (Pervasives.compare (make_test_value 0) (make_test_value 0));
    false
  with Invalid_argument "equal: functional value" ->
    true

let tests = [
  "comparison", test_comparison;
]
