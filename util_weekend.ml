(*
   Add or subtract work days to a date, skipping weekends.

   It could be extended to support holidays or other weekend definitions
   than Saturday-Sunday.
*)

module Param = struct
  type date = Util_dateonly.t * Util_timeonly.t

  let deconstruct (date_only, time_only) =
    ((date_only, Util_timeonly.of_float 0.),
     Util_timeonly.to_float time_only)

  let reconstruct (date_only, old_time_only) time_only =
    assert (Util_timeonly.to_float old_time_only = 0.);
    (date_only, Util_timeonly.of_float time_only)

  let add_day (date_only, time_only) =
    (Util_dateonly.next_day date_only, time_only)

  let sub_day (date_only, time_only) =
    (Util_dateonly.previous_day date_only, time_only)
end

module Workday = Util_workday.Make (Param)

let is_workday (date_only, time_only) =
  match Util_dateonly.day_of_the_week date_only with
  | 0 | 6 ->
      (* Sunday | Saturday *)
      false
  | 1 | 2 | 3 | 4 | 5 ->
      (* Monday-Friday *)
      true
  | _ -> assert false

let to_localtime ~timezone t =
  let local = Util_timezone.local_of_utc timezone t in
  Util_localtime.to_pair local

let of_localtime ~timezone (date_only, time_only) =
  let local = Util_localtime.of_pair date_only time_only in
  Util_timezone.utc_of_local timezone local

(*
   Add workday-time (in seconds) to a date.
*)
let add ~timezone t workday_time =
  of_localtime ~timezone (
    Workday.add
      ~is_workday
      (to_localtime ~timezone t)
      workday_time
  )

(*
   Subtract workday-time (in seconds) from a date.
*)
let sub ~timezone t workday_time =
  of_localtime ~timezone (
    Workday.sub
      ~is_workday
      (to_localtime ~timezone t)
      workday_time
  )

let test_weekend () =
  (* Daylight savings time change in California on March 12, 2017. *)
  let timezone = "America/Los_Angeles" in

  (* Dates across the time change. Note the time of the day is the same
     despite the time change. *)
  let t1 = Util_time.of_string "2017-03-10T09:00:00-08:00" in
  let t2 = Util_time.of_string "2017-03-17T09:00:00-07:00" in

  let eq a b = Util_time.compare a b = 0 in

  let time_shift =
    (Util_time.to_float t2 -. Util_time.to_float t1) -. 7. *. 86400.
  in
  assert (time_shift = -3600.);

  (* Subtract 5 workdays from t2 *)
  assert (
    eq t1 (
      sub
        ~timezone
        t2
        (5. *. 86400.)
    )
  );

  (* Add 5 workdays to t1 *)
  assert (
    eq t2 (
      add
        ~timezone
        t1
        (5. *. 86400.)
    )
  );

  true

let tests = [
  "weekend", test_weekend;
]
