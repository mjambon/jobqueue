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

(*
   Compute the mean of the k or (k+1) values around the median.
   `k` must be odd.
   `median_k 1` computes the regular median.
*)
let median_k k l =
  if l = [] then
    invalid_arg "Util_float.median_k: empty list";
  if k <= 0 || k mod 2 = 0 then
    invalid_arg "Util_float.median_k: k must be a positive odd number";
  let a = Array.of_list l in
  Array.sort compare a;
  let n0 = Array.length a in
  let span =
    let k =
      if n0 mod 2 = 0 then
        k + 1
      else
        k
    in
    min k n0
  in
  let acc = ref 0. in
  let start = (n0 - span) / 2 in
  for i = start to start + span - 1 do
    acc := !acc +. a.(i)
  done;
  !acc /. float span

let median l = median_k 1 l
let median3 l = median_k 3 l
let median5 l = median_k 5 l

let test_median_k () =
  assert (median [1.] = 1.);
  assert (median [1.; 2.] = 3. /. 2.);
  assert (median [1.; 2.; 10.] = 2.);
  assert (median3 [0.; 1.; 10.] = 11. /. 3.);
  assert (median3 [0.; 1.] = 1. /. 2.);
  assert (median3 [0.; 1.; 10.; 100.] = 111. /. 4.);
  assert (median3 [0.; 1.; 10.; 100.; 1000.] = 111. /. 3.);
  true

let tests = [
  "round", test_round;
  "equal_rel", test_equal_rel;
  "equal_abs", test_equal_abs;
  "positive_mod", test_positive_mod;
  "median_k", test_median_k;
]
