open Printf

type t = {
  year : int;
  month : int;
  day : int;
  hour : int;
  min : int;
  sec : float;
  string : string;
}

let invalid label value =
  invalid_arg (sprintf "Util_localtime.create: invalid %s %s" label value)

let invalid_int label n =
  invalid label (string_of_int n)

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
  let string =
    (* RFC 3339 timestamp - the timezone suffix 'Z' is for compliance only *)
    sprintf "%04d-%02d-%02dT%02d:%02d:%06.3fZ"
      year month day hour min sec
  in
  {
    year; month; day; hour; min; sec;
    string;
  }

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

let test_conversions () =
  let conv s = to_string (of_string s) in
  assert (conv "1996-12-20T00:39:57Z" = "1996-12-20T00:39:57.000Z");
  assert (conv "12345-12-20T23:59:57.56789" = "12345-12-20T23:59:57.568Z");
  true

let tests = [
  "conversions", test_conversions;
]
