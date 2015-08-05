type t = {
  unixtime : float;
  string : string;
}

let round x =
  floor (x +. 0.5)

let round_milliseconds x =
  1e-3 *. round (1e3 *. x)

let test_round_milliseconds () =
  assert (round_milliseconds 8.01 = 8.01);
  assert (round_milliseconds 3.0001 = 3.);
  assert (round_milliseconds 123.4564 = 123.456);
  assert (round_milliseconds 123.452 = 123.452);
  assert (round_milliseconds 2.998 = 2.998);
  assert (round_milliseconds 2.9998 = 3.);
  assert (round_milliseconds (-1.9999) = -2.);
  assert (round_milliseconds (-1.0001) = -1.);
  assert (round_milliseconds (-1.23451) = -1.235);
  assert (round_milliseconds (-1.23449) = -1.234);
  assert (round_milliseconds 1385625519.658 = 1385625519.658);
  true

let of_float t =
  let t = round_milliseconds t in
  {
    unixtime = t;
    string =
      Nldate.mk_internet_date
        ~localzone:true ~digits:3
        (t +. 0.0005);
  }

let to_float x = x.unixtime

let parse s =
  try
    Some (of_float (Nldate.since_epoch_approx (Nldate.parse s)))
  with _ -> None

let of_string s =
  match parse s with
      None -> invalid_arg ("Util_time.of_string_exn: " ^ s)
    | Some x -> x

let to_string x = x.string

let format ~fmt x =
  Nldate.mk_date ~fmt x.unixtime

let now () = of_float (Unix.gettimeofday ())

let is_past x = x.unixtime < Unix.gettimeofday ()
let is_future x = x.unixtime > Unix.gettimeofday ()

let add x seconds = of_float (x.unixtime +. seconds)
let sub x seconds = of_float (x.unixtime -. seconds)

let add_min x minutes =
  add x (minutes *. 60.)

let sub_min x minutes =
  sub x (minutes *. 60.)

let add_hour x hours =
  add x (hours *. 3600.)

let sub_hour x hours =
  sub x (hours *. 3600.)

let add_day x days =
  add x (days *. 86400.)

let sub_day x days =
  sub x (days *. 86400.)

let set_min time min =
  let t = Unix.localtime time.unixtime in
  let (u, _) = Unix.mktime {t with Unix.tm_min = min} in
  of_float u

let set_sec time sec =
  let t = Unix.localtime time.unixtime in
  let (u, _) = Unix.mktime {t with Unix.tm_sec = sec} in
  of_float u

let test_add () =
  let time = of_float 1424225951. in
  let day_add = add_day time 9. in
  let hour_add = add_hour time 9. in
  let min_add = add_min time 9. in
  let sec_add = add time 9. in
  assert (day_add.unixtime = 1425003551.);
  assert (hour_add.unixtime = 1424258351.);
  assert (min_add.unixtime = 1424226491.);
  assert (sec_add.unixtime = 1424225960.);
  true

let test_set () =
  let time = of_float 1424225951. in
  let min_set = set_min time 99 in
  let sec_set = set_sec time 99 in
  assert (min_set.unixtime = 1424230751.);
  assert (sec_set.unixtime = 1424226039.);
  true


(* add a millisecond, or more if it's not enough to change the string
   representation. *)
let next x =
  let rec aux x increment =
    let x' = add x increment in
    if x'.string <> x.string then x'
    else aux x (1.4 *. increment)
  in
  aux x 0.001

let wrap = of_string
let unwrap = to_string

let compare a b = Pervasives.compare a.unixtime b.unixtime
let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b

let hour_of_day h date_time =
  let gmtime = Unix.gmtime date_time.unixtime in
  let time = {Unix.tm_sec = 0;
    tm_min = 0;
    tm_hour = h;
    tm_mday = gmtime.Unix.tm_mday;
    tm_mon = gmtime.Unix.tm_mon;
    tm_year = gmtime.Unix.tm_year;
    tm_wday = gmtime.Unix.tm_wday;
    tm_yday = gmtime.Unix.tm_yday;
    tm_isdst = gmtime.Unix.tm_isdst} in
  let (unixtime, _) = Unix.mktime time in
  of_float unixtime

let diff_seconds a b =
  let a_float = to_float a in
  let b_float = to_float b in
  let diff_seconds = a_float -. b_float in
  diff_seconds

let diff_days a b =
  let diff_seconds = diff_seconds a b in
  let diff_days = diff_seconds /. 86400. in
  diff_days

let test_diffs () =
  let eql a b =
    abs_float (a -. b) < 0.001 in
  let cur = now () in
  let ten_minutes_from_now = add_min cur 10. in
  let ten_days_from_now = add_day cur 10. in
  assert (eql (diff_seconds ten_minutes_from_now cur) 600.);
  assert (eql (diff_days ten_days_from_now cur) 10.);
  true

let test_recover () =
  let conv t =
    let x1 = of_float t in
    let x2 = of_string (to_string x1) in
    x1 = x2
  in
  assert (conv 0.);
  assert (conv 0.1000);
  assert (conv 0.1001);
  assert (conv 0.1002);
  assert (conv 0.1003);
  true

module As_unixtime = struct
  type time = t
  type t = time

  let round_down = floor
  let of_float = of_float

  let to_float x =
    round_down x.unixtime

  let wrap = of_float
  let unwrap = to_float

  let of_string s =
    of_float (float_of_string s)

  let to_string x =
    Printf.sprintf "%.0f" (to_float x)

  let test_unixtime () =
    assert (round_down 0.1 = 0.);
    assert (round_down 0.9 = 0.);
    assert (round_down (-0.1) = -1.);
    assert (round_down (-10.1) = -11.);
    assert (to_float (of_float 3.5) = 3.);
    assert (to_string (of_float 3.5) = "3");
    assert ((of_string "77.333").unixtime > 77.);
    assert (to_float (of_string "77.333") = 77.);
    assert (to_string (of_string "77.333") = "77");
    true
end

let ( = ) a b = compare a b = 0
let ( < ) a b = compare a b < 0
let ( > ) a b = compare a b > 0
let ( <= ) a b = compare a b <= 0
let ( >= ) a b = compare a b >= 0

let tests = [
  "round milliseconds", test_round_milliseconds;
  "recover", test_recover;
  "unixtime", As_unixtime.test_unixtime;
  "update times", test_add;
  "set times", test_set;
  "diffs", test_diffs;
]

