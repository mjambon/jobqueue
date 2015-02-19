open Printf

type t = float

let equal = ( = )
let compare = compare

let to_string x =
  sprintf "%.0f" x

(*
   Round up or down to the nearest integer.
*)
let round x =
  if x < 0. then
    let y = ceil (x -. 0.5) in
    if y = -0. then 0.
    else y
  else
    floor (x +. 0.5)

let of_string s =
  round (float_of_string s)

let of_float x =
  round x

let to_float x =
  x

let wrap = of_float
let unwrap = to_float

let of_time x =
  Util_time.to_float x

let to_time x =
  Util_time.of_float x

let now () =
  Unix.time ()

let add t seconds =
  of_float (t +. seconds)

let sub t seconds =
  add t (-. seconds)

let add_year time year =
  let t = Unix.localtime time in
  let (u, _) = Unix.mktime {t with Unix.tm_year = t.Unix.tm_year + year} in
  u

let add_mon time mon =
  let t = Unix.localtime time in
  let (u, _) = Unix.mktime {t with Unix.tm_mon = t.Unix.tm_mon + mon} in
  u

let add_day time day =
  let t = Unix.localtime time in
  let (u, _) = Unix.mktime {t with Unix.tm_mday = t.Unix.tm_mday + day} in
  u

let add_hour time hour =
  let t = Unix.localtime time in
  let (u, _) = Unix.mktime {t with Unix.tm_hour = t.Unix.tm_hour + hour} in
  u

let add_min time min =
  let t = Unix.localtime time in
  let (u, _) = Unix.mktime {t with Unix.tm_min = t.Unix.tm_min + min} in
  u

let add_sec time sec =
  let t = Unix.localtime time in
  let (u, _) = Unix.mktime {t with Unix.tm_sec = t.Unix.tm_sec + sec} in
  u

let set_year time year =
  let t = Unix.localtime time in
  let (u, _) = Unix.mktime {t with Unix.tm_year = year} in
  u

let set_mon time mon =
  let t = Unix.localtime time in
  let (u, _) = Unix.mktime {t with Unix.tm_mon = mon} in
  u

let set_day time day =
  let t = Unix.localtime time in
  let (u, _) = Unix.mktime {t with Unix.tm_mday = day} in
  u

let set_hour time hour =
  let t = Unix.localtime time in
  let (u, _) = Unix.mktime {t with Unix.tm_hour = hour} in
  u

let set_min time min =
  let t = Unix.localtime time in
  let (u, _) = Unix.mktime {t with Unix.tm_min = min} in
  u

let set_sec time sec =
  let t = Unix.localtime time in
  let (u, _) = Unix.mktime {t with Unix.tm_sec = sec} in
  u

let test_add () =
  let time = 1424225951. in
  let year_add = add_year time 9 in
  let mon_add = add_mon time 9 in
  let day_add = add_day time 9 in
  let hour_add = add_hour time 9 in
  let min_add = add_min time 9 in
  let sec_add = add_sec time 9 in
  assert (year_add = 1708222751.);
  assert (mon_add = 1447813151.);
  assert (day_add = 1425003551.);
  assert (hour_add = 1424258351.);
  assert (min_add = 1424226491.);
  assert (sec_add = 1424225960.);
  true

let test_set () =
  let time = 1424225951. in
  let year_set = set_year time 99 in
  let mon_set = set_mon time 99 in
  let day_set = set_day time 99 in
  let hour_set = set_hour time 99 in
  let min_set = set_min time 99 in
  let sec_set = set_sec time 99 in
  assert (year_set = 919304351.);
  assert (mon_set = 1681780751.);
  assert (day_set = 1431307151.);
  assert (hour_set = 1424517551.);
  assert (min_set = 1424230751.);
  assert (sec_set = 1424226039.);
  true

let tests = [
  "update times", test_add;
  "set times", test_set;
]
