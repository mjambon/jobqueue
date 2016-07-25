(*
   Miscellaneous utilities dealing with floats.
*)

(*
   Round a float to the nearest int
*)
let round x =
  if x > 0. then
    truncate (x +. 0.5)
  else
    truncate (x -. 0.5)

let test_round () =
  assert (round 123.45 = 123);
  assert (round 0.6 = 1);
  assert (round 0.4 = 0);
  assert (round (-0.4) = 0);
  assert (round (-0.6) = -1);
  true

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

let ( =~ ) a b = equal_rel a b

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

(* Like mod_float, but return a number between 0 and m, even for negative x. *)
let positive_mod x m =
  if not (m > 0.) then
    invalid_arg "Util_timeonly.modulo";
  if x >= 0. then
    mod_float x m
  else
    mod_float x m +. m

let test_positive_mod () =
  assert (positive_mod 17. 4. =~ 1.);
  assert (positive_mod 15. 4. =~ 3.);
  assert (positive_mod (-5.) 4. =~ 3.);
  true

let tests = [
  "round", test_round;
  "equal_rel", test_equal_rel;
  "equal_abs", test_equal_abs;
  "positive_mod", test_positive_mod;
]
