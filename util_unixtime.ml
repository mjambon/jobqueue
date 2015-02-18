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

let update_year year t =
  {t with Unix.tm_year = t.Unix.tm_year + year}

let update_mon mon t =
  {t with Unix.tm_mon = t.Unix.tm_mon + mon}

let update_day day t =
  {t with Unix.tm_mday = t.Unix.tm_mday + day}

let update_hour hour t =
  {t with Unix.tm_hour = t.Unix.tm_hour + hour}

let update_min min t =
  {t with Unix.tm_min = t.Unix.tm_min + min}

let update_sec sec t =
  {t with Unix.tm_sec = t.Unix.tm_sec + sec}

let set_year year t =
  {t with Unix.tm_year = year}

let set_mon mon t =
  {t with Unix.tm_mon = mon}

let set_day day t =
  {t with Unix.tm_mday = day}

let set_hour hour t =
  {t with Unix.tm_hour = hour}

let set_min min t =
  {t with Unix.tm_min = min}

let set_sec sec t =
  {t with Unix.tm_sec = sec}

let test_update () =
  let time = {Unix.tm_sec = 11;
    tm_min = 19;
    tm_hour = 18;
    tm_mday = 17;
    tm_mon = 1;
    tm_year = 115;
    tm_wday = 2;
    tm_yday = 47;
    tm_isdst = false}
  in
  let (year_update, _) = Unix.mktime (update_year 9 time) in
  let (mon_update, _) = Unix.mktime (update_mon 9 time) in
  let (day_update, _) = Unix.mktime (update_day 9 time) in
  let (hour_update, _) = Unix.mktime (update_hour 9 time) in
  let (min_update, _) = Unix.mktime (update_min 9 time) in
  let (sec_update, _) = Unix.mktime (update_sec 9 time) in
  assert (year_update = 1708222751.);
  assert (mon_update = 1447813151.);
  assert (day_update = 1425003551.);
  assert (hour_update = 1424258351.);
  assert (min_update = 1424226491.);
  assert (sec_update = 1424225960.);
  true

let test_set () =
  let time = {Unix.tm_sec = 11;
    tm_min = 19;
    tm_hour = 18;
    tm_mday = 17;
    tm_mon = 1;
    tm_year = 115;
    tm_wday = 2;
    tm_yday = 47;
    tm_isdst = false}
  in
  let (year_set, _) = Unix.mktime (set_year 99 time) in
  let (mon_set, _) = Unix.mktime (set_mon 99 time) in
  let (day_set, _) = Unix.mktime (set_day 99 time) in
  let (hour_set, _) = Unix.mktime (set_hour 99 time) in
  let (min_set, _) = Unix.mktime (set_min 99 time) in
  let (sec_set, _) = Unix.mktime (set_sec 99 time) in
  assert (year_set = 919304351.);
  assert (mon_set = 1681780751.);
  assert (day_set = 1431307151.);
  assert (hour_set = 1424517551.);
  assert (min_set = 1424230751.);
  assert (sec_set = 1424226039.);
  true

let tests = [
  "update times", test_update;
  "set times", test_set;
]
