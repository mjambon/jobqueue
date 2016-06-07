(*
   Equality between two numbers, using relative precision
*)
let equal_rel ?(prec = 1e-6) a b =
  if prec <> prec then
    invalid_arg "Util_float.equal_rel: NaN precision"
  else if prec < 0. then
    invalid_arg "Util_float.equal_rel: negative precision";
  abs_float (a -. b) <= prec *. max (abs_float a) (abs_float b)

let test_equal_rel () =
  assert (equal_rel 0. 0.);
  assert (equal_rel ~prec:0.1 100. 101.);
  assert (not (equal_rel ~prec:0.1 1. 2.));
  assert (equal_rel ~prec:0.1 (-100.) (-101.));
  assert (equal_rel ~prec:0. 1. 1.);
  assert (equal_rel ~prec:0. 0. 0.);
  true

(*
   Equality between two numbers, using absolute precision
*)
let equal_abs ?(prec = 1e-6) a b =
  if prec <> prec then
    invalid_arg "Util_float.equal_abs: NaN precision"
  else if prec < 0. then
    invalid_arg "Util_float.equal_abs: negative precision";
  abs_float (a -. b) <= prec

let test_equal_abs () =
  assert (equal_abs 0. 0.);
  assert (equal_abs ~prec:0. 1. 1.);
  assert (not (equal_abs ~prec:0.1 1. 2.));
  assert (not (equal_abs ~prec:0.1 (-1.) 1.));
  assert (equal_abs ~prec:0.1 (-1.) (-1.05));
  true

let tests = [
  "equal_rel", test_equal_rel;
  "equal_abs", test_equal_abs;
]
