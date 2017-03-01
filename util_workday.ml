(*
   Compute dates by adding/removing work days.

   This is a functor with no dependency so that it can be reused
   with any date/time library.
*)

open Printf

let day_length = 86400.
  (* This is 24 hours expressed in seconds.
     Note that some calendar days last 23 hours or 25 hours
     due to "daylight savings" or other timezone policy changes. *)

module type Param = sig
  type date
    (* User-provided date type *)

  val deconstruct : date -> (date * float)
    (* Decompose a date into the start of the day and the number of
       seconds from the start of the day. *)

  val reconstruct : date -> float -> date
    (* Add seconds (less than 86400) to a date that represents
       the start of a day. *)

  val add_day : date -> date
  val sub_day : date -> date
    (* Add or subtract a whole day to a date.
       It is fine if it adds a calendar day
       (which may be 23 or 25 hours) instead of always 24 hours. *)
end

module type S = sig
  include Param

  (*
     Add or subtract time to a date, skipping days
     where `is_workday` returns false.
  *)
  val add : is_workday:(date -> bool) -> date -> float -> date
  val sub : is_workday:(date -> bool) -> date -> float -> date
end

(*
   Given p an arbitrary real and q a positive real,
   compute a and b such that:

     a is an integer
     0 <= b < q
     p = a * q + b
*)
let rec div p q =
  assert (q > 0.);
  let r = p /. q in
  let frac, int = modf r in
  if frac >= 0. then
    truncate int, q *. frac
  else if frac = 0. then
    truncate int, 0.
  else
    truncate int - 1, q *. (frac +. 1.)

let test_div () =
  let eq_float a b = abs_float (a -. b) < 0.00001 in
  let ( =~ ) (a1, b1) (a2, b2) =
    a1 = a2 && eq_float b1 b2
  in
  assert (div 3. 1. =~ (3, 0.));
  assert (div 3.2 1. =~ (3, 0.2));
  assert (div 3.2 3. =~ (1, 0.2));
  assert (div (-3.) 1. =~ (-3, 0.));
  assert (div (-3.2) 1. =~ (-4, 0.8));
  assert (div (-3.2) 3. =~ (-2, 2.8));
  true

module Make (Param : Param) : S with type date = Param.date = struct
  include Param

  let add ~is_workday from worktime =
    (* t0 is the start of the current day,
       dt0 is the duration elapsed on that day. *)
    let t0, dt0 = deconstruct from in
    let worktime_from_t0 =
      if is_workday t0 then
        dt0 +. worktime
      else
        worktime
    in
    let offset_workdays, offset_seconds = div worktime_from_t0 day_length in
    (* offset_seconds is the desired time-only.
       offset_workdays is the number of whole workdays to add to t0. *)
    assert (offset_seconds >= 0.);

    (* t1 is the date we have to shift by a whole number of days. *)
    let t1 = reconstruct t0 offset_seconds in

    let rec add_days t offset_workdays =
      assert (offset_workdays >= 0);
      if is_workday t then
        if offset_workdays = 0 then t
        else
          add_days (add_day t) (offset_workdays - 1)
      else
        add_days (add_day t) offset_workdays
    in

    let rec sub_days t offset_workdays =
      assert (offset_workdays <= 0);
      if is_workday t then
        if offset_workdays = 0 then t
        else
          sub_days (sub_day t) (offset_workdays + 1)
      else
        sub_days (sub_day t) offset_workdays
    in

    (* t2 is the result. *)
    let t2 =
      if offset_workdays >= 0 then
        add_days t1 offset_workdays
      else
        sub_days t1 offset_workdays
    in

    t2

  let sub ~is_workday from worktime =
    add ~is_workday from (-.worktime)
end

module Test_param = struct
  type date = float

  let time_only t =
    if t >= 0. then
      mod_float t day_length
    else
      mod_float t day_length +. day_length

  let deconstruct t =
    let dt = time_only t in
    t -. time_only t, dt

  let reconstruct t dt =
    t +. dt

  let add_day t = t +. day_length
  let sub_day t = t -. day_length
end

module Test = Make (Test_param)

let test_workdays () =
  let open Test_param in

  let round x =
    if x >= 0. then truncate (x +. 0.5)
    else truncate (x -. 0.5)
  in
  assert (round 1.4 = 1);
  assert (round 1.6 = 2);
  assert (round (-1.4) = -1);
  assert (round (-1.6) = -2);

  let start_of_day t = fst (deconstruct t) in

  (* Define weekends as: 0, 1, 7, 8, 14, 15, etc. *)
  let is_workday t =
    let day = round (start_of_day t /. day_length) in
    match day mod 7 with
    | 0 | 1 -> false
    | _ -> true
  in
  assert (not (is_workday 0.));
  assert (not (is_workday day_length));
  assert (not (is_workday (2. *. day_length -. 1.)));
  assert (is_workday (2. *. day_length));

  let saturday0 = 0. in
  let _sunday1 = day_length in
  let monday2 = 2. *. day_length in
  let thursday5 = 5. *. day_length in

  let add t dt =
    let t2 = Test.add ~is_workday t dt in
    printf "add %.0f %.0f -> %.0f\n%!" t dt t2;
    t2
  in
  let sub t dt =
    let t2 = Test.sub ~is_workday t dt in
    printf "sub %.0f %.0f -> %.0f\n%!" t dt t2;
    t2
  in
  let print x =
    printf "%.0f\n%!" x;
    x
  in

  assert (add monday2 1. = monday2 +. 1.);
  assert (add saturday0 1. = add monday2 1.);
  assert (add (thursday5 +. 100.) (5. *. day_length)
          = thursday5 +. 100. +. 7. *. day_length);

  assert (sub (monday2 +. 1.) 1. = monday2);
  assert (sub (monday2 +. 1.) 2. = saturday0 -. 1.);
  assert (sub (thursday5 +. 100.) (5. *. day_length)
          = print (thursday5 +. 100. -. 7. *. day_length));

  true

let tests = [
  "div", test_div;
  "workdays", test_workdays;
]
