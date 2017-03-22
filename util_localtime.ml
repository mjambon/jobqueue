(*
   See util_localtime.mli
*)

open Printf

type t = {
  year: int;
  month: int;
  day: int;
  hour: int;
  min: int;
  sec: float;
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
    tm_wday = 0; (* ignored by Unix.mktime *)
    tm_yday = 0; (* ignored by Unix.mktime *)
    tm_isdst = false; (* ignored by Unix.mktime *)
  }

(*
   Return seconds since 1970-01-01 and a normalized tm
*)
let recompute_absolute_time x =
  Unix.mktime (to_unnormalized_unix_tm x)

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
    tm_wday = 0; (* ignored by Unix.mktime *)
    tm_yday = 0; (* ignored by Unix.mktime *)
    tm_isdst = false; (* ignored by Unix.mktime *)
  } in
  (* normalize and complete missing fields *)
  let t, tm = Unix.mktime tm in
  tm, fpart x.sec

let of_unix_tm unnormalized_tm subsecond =
  if not (subsecond >= 0. && subsecond < 1.) then
    invalid_arg ("Util_localtime.of_unix_tm: out-of-range subsecond "
                  ^ string_of_float subsecond);
  let open Unix in
  let t, tm = Unix.mktime unnormalized_tm in
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

let of_float t =
  let x = Unix.gmtime t in
  of_unix_tm x (fpart t)

let to_float x =
  x.absolute_time

let create ~year ~month ~day ~hour ~min ~sec =
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

let of_string s =
  Scanf.sscanf s "%d-%d-%dT%d:%d:%f"
    (fun year month day hour min sec ->
       create ~year ~month ~day ~hour ~min ~sec)

let of_string_opt s =
  try Some (of_string s)
  with _ -> None

let to_string x = x.string

let wrap = of_string
let unwrap = to_string

let of_pair date time =
  create
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

let test_conversions () =
  let conv s = to_string (of_string s) in
  assert (conv "1996-12-20T00:39:57Z" = "1996-12-20T00:39:57.000");
  assert (conv "12345-12-20T23:59:57.56789" = "12345-12-20T23:59:57.568");
  true

let format ~fmt x =
  Nldate.mk_date ~fmt (to_float x)

let tests = [
  "conversions", test_conversions;
]
