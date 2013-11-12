open Printf

type t = {
  hour : int;
  min : int;
  sec : float;
  string : string;
}

let invalid label value =
  invalid_arg (sprintf "Util_timeonly.create: invalid %s %s" label value)

let invalid_int label n =
  invalid label (string_of_int n)

let create ~hour ~min ~sec =
  if hour < 0 || hour > 23 then
    invalid_int "hour" hour;
  if min < 0 || min > 59 then
    invalid_int "min" min;
  if not (sec >= 0. && sec < 60.) then
    invalid "sec" (sprintf "%g" sec);
  let string =
    (* RFC 3339 "time" *)
    sprintf "%02d:%02d:%06.3f" hour min sec
  in
  {
    hour; min; sec;
    string;
  }

let of_string s =
  Scanf.sscanf s "%d:%d:%f"
    (fun hour min sec -> create ~hour ~min ~sec)

let of_string_opt s =
  try Some (of_string s)
  with _ -> None

let to_string x = x.string

let wrap = of_string
let unwrap = to_string

let to_float { hour; min; sec } =
  float hour *. 3600. +. float min *. 60. +. sec

let of_float x =
  if not (x >= 0. && x < 86400.) then
    invalid_arg "Util_timeonly.of_float";
  let sec = mod_float x 60. in
  let min = truncate (mod_float x 3600. /. 60.) in
  let hour = truncate (x /. 3600.) in
  create ~hour ~min ~sec

let test_conversions () =
  let conv s = to_string (of_string s) in
  assert (conv "00:39:57" = "00:39:57.000");
  assert (conv "23:59:57.56789" = "23:59:57.568");
  true

let test_float () =
  let x = of_float 86399.99 in
  assert (x.hour = 23);
  assert (x.min = 59);
  assert (x.sec > 59.9 && x.sec < 60.);
  let t = to_float x in
  assert (t > 86399.9 && t < 86400.);
  true

let tests = [
  "conversions", test_conversions;
  "float", test_float;
]
