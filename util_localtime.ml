(*
   See util_localtime.mli
*)

open Printf

type t = {
  (* Core fields *)
  timezone: Util_timezone.t;
  year: int;
  month: int;
  day: int;
  hour: int;
  min: int;
  sec: float;

  (* Derivable fields *)
  string: string;
  wday: int;
  yday: int;
  absolute_time: float;
  absolute_day: int;
}

let invalid label value =
  invalid_arg (sprintf "Util_localtime.create: invalid %s %s" label value)

let invalid_int label n =
  invalid label (string_of_int n)

(*
   0.     -> 0
   1.     -> 0
   86399. -> 0
   86400. -> 1

   -86400. -> -1
   -86399. -> -1
   -1.     -> -1
   0.      ->  0
*)
let absolute_day_of_float x =
  truncate (floor (x /. 86400.))

(*
   Return what needs to be subtracted in order to round down to the
   nearest integer, i.e. x -> (x -. floor x).

   1.3 -> 0.3
   -1.3 -> 0.7
*)
let fpart x =
  x -. floor x

let to_unnormalized_unix_tm x =
  {
    Unix.tm_sec = truncate x.sec;
    tm_min = x.min;
    tm_hour = x.hour;
    tm_mday = x.day;
    tm_mon = x.month - 1;
    tm_year = x.year - 1900;
    tm_wday = 0; (* ignored by mktime *)
    tm_yday = 0; (* ignored by mktime *)
    tm_isdst = false; (* ignored by mktime *)
  }

(*
   Return seconds since 1970-01-01 and a normalized tm
*)
let recompute_absolute_time x =
  Util_timezone.mktime x.timezone (to_unnormalized_unix_tm x)

(* RFC 3339 timestamp - without timezone suffix *)
let make_string year month day hour min sec =
  sprintf "%04d-%02d-%02dT%02d:%02d:%06.3f"
    year month day hour min sec

let to_unix_tm x =
  let tm = {
    Unix.tm_sec = truncate x.sec;
    tm_min = x.min;
    tm_hour = x.hour;
    tm_mday = x.day;
    tm_mon = x.month - 1;
    tm_year = x.year - 1900;
    tm_wday = 0; (* ignored by mktime *)
    tm_yday = 0; (* ignored by mktime *)
    tm_isdst = false; (* ignored by mktime *)
  } in
  let unixtime, tm = Util_timezone.mktime x.timezone tm in
  tm, fpart x.sec

let of_unix_tm ~timezone unnormalized_tm subsecond =
  if not (subsecond >= 0. && subsecond < 1.) then
    invalid_arg ("Util_localtime.of_unix_tm: out-of-range subsecond "
                  ^ string_of_float subsecond);
  let open Unix in
  let t, tm = Util_timezone.mktime timezone unnormalized_tm in
  let year = 1900 + tm.tm_year in
  let month = 1 + tm.tm_mon in
  let day = tm.tm_mday in
  let hour = tm.tm_hour in
  let min = tm.tm_min in
  let sec = float tm.tm_sec +. subsecond in
  let string = make_string year month day hour min sec in
  let wday = tm.tm_wday in
  let yday = tm.tm_yday in
  {
    timezone;
    year;
    month;
    day;
    hour;
    min;
    sec;
    string;
    wday;
    yday;
    absolute_time = t +. subsecond;
    absolute_day = absolute_day_of_float t;
  }

let create ~timezone ~year ~month ~day ~hour ~min ~sec =
  if month < 1 || month > 12 then
    invalid_int "month" month;
  if day < 1 || day > 31 then (* TODO: check for months shorter than 31 days *)
    invalid_int "day" day;
  if hour < 0 || hour > 23 then
    invalid_int "hour" hour;
  if min < 0 || min > 59 then
    invalid_int "min" min;
  if not (sec >= 0. && sec < 60.) then
    invalid "sec" (sprintf "%g" sec);
  let string = make_string year month day hour min sec in
  let tmp = {
    timezone;
    year; month; day; hour; min; sec;
    string;
    (* Fields to be fixed in the next step *)
    wday = 0;
    yday = 0;
    absolute_time = 0.;
    absolute_day = 0;
  } in
  let absolute_time, tm = recompute_absolute_time tmp in
  let absolute_day = absolute_day_of_float absolute_time in
  let open Unix in
  { tmp with
    wday = tm.tm_wday;
    yday = tm.tm_yday;
    absolute_time;
    absolute_day }

let set_time ?hour ?min ?sec x =
  let hour =
    match hour with
    | None -> x.hour
    | Some h -> h
  in
  let min =
    match min with
    | None -> x.min
    | Some h -> h
  in
  let sec =
    match sec with
    | None -> x.sec
    | Some h -> h
  in
  if hour < 0 || hour > 23
     || min < 0 || min > 59
     || not (sec >= 0. && sec < 60.) then
    invalid_arg "Util_localtime.set_time";

  { x with hour; min; sec }

let of_string ~timezone s =
  Scanf.sscanf s "%d-%d-%dT%d:%d:%f"
    (fun year month day hour min sec ->
       create ~timezone ~year ~month ~day ~hour ~min ~sec)

let of_string_opt ~timezone s =
  try Some (of_string ~timezone s)
  with _ -> None

let to_string x = x.string

let of_pair ~timezone date time =
  create
    ~timezone
    ~year: date.Util_dateonly.year
    ~month: date.Util_dateonly.month
    ~day: date.Util_dateonly.day
    ~hour: time.Util_timeonly.hour
    ~min: time.Util_timeonly.min
    ~sec: time.Util_timeonly.sec

let dateonly {year; month; day} =
  Util_dateonly.create ~year ~month ~day

let timeonly {hour; min; sec} =
  Util_timeonly.create ~hour ~min ~sec

let to_pair x =
  (dateonly x, timeonly x)

let test_string_conversions () =
  let timezone = Util_timezone.of_string "Pacific/Honolulu" in
  let conv s = to_string (of_string ~timezone s) in
  assert (conv "1996-12-20T00:39:57Z" = "1996-12-20T00:39:57.000");
  assert (conv "12345-12-20T23:59:57.56789" = "12345-12-20T23:59:57.568");
  true

let to_float local =
  local.absolute_time

let of_float ~timezone unixtime =
  let x = Util_timezone.localtime timezone unixtime in
  of_unix_tm ~timezone x (fpart unixtime)

let to_utc local =
  Util_time.of_float (to_float local)

let of_utc ~timezone utc =
  of_float ~timezone (Util_time.to_float utc)

let midnight = Util_timeonly.create ~hour:0 ~min:0 ~sec:0.

let of_day ~timezone day =
  of_pair ~timezone day midnight

let test_utc_of_local () =
  let timezone = Util_timezone.of_string "America/New_York" in
  let local = of_string ~timezone "2016-02-13T00:39:57" in
  let utc = to_utc local in
  let out = Util_time.to_float utc in
  let expected =
    Util_time.to_float (Util_time.of_string "2016-02-12T21:39:57.000-08:00")
  in
  out = expected

let test_local_of_utc () =
  let timezone = Util_timezone.of_string "America/New_York" in
  let utc = Util_time.of_string "2016-02-13T00:39:57Z" in
  let local = of_utc ~timezone utc in
  let out = to_string local in
  let expected = "2016-02-12T19:39:57.000" in
  out = expected

let test_timezones () =
  let tz1 = Util_timezone.of_string "America/Los_Angeles" in
  let tz2 = Util_timezone.of_string "America/New_York" in
  let t0 = 1490312238. in
  let utc0 = Util_time.of_float 1490312238. in

  let local1 = of_utc ~timezone:tz1 utc0 in
  let utc1 = to_utc local1 in
  assert (Util_time.to_float utc1 = t0);

  let local2 = of_utc ~timezone:tz2 utc1 in
  let utc2 = to_utc local2 in
  assert (Util_time.to_float utc2 = t0);

  assert (local1.hour = 16);
  assert (local2.hour = 19);

  assert (to_string local1 = "2017-03-23T16:37:18.000");
  assert (to_string local2 = "2017-03-23T19:37:18.000");

  true


let format ~fmt x =
  Util_timezone.with_timezone x.timezone (fun () ->
    Nldate.mk_date ~localzone:true ~fmt (to_float x)
  )

module No_timezone = struct
  type localtime = t
  type t = localtime

  let timezone = Util_timezone.utc

  let of_string = of_string ~timezone
  let of_string_opt = of_string_opt ~timezone
  let to_string = to_string

  let wrap = of_string
  let unwrap = to_string

  let create = create ~timezone
  let set_time = set_time

  let of_pair = of_pair ~timezone
  let to_pair = to_pair

  let dateonly = dateonly
  let timeonly = timeonly

  let format = format
end

let tests = [
  "string conversions", test_string_conversions;
  "local -> utc", test_utc_of_local;
  "utc -> local", test_local_of_utc;
  "timezones", test_timezones;
]
