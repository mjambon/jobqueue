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
  true

let of_float t =
  {
    unixtime = t;
    string =
      Nldate.mk_internet_date
        ~localzone:true ~digits:3
        (round_milliseconds t);
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

let add x seconds = of_float (x.unixtime +. seconds)
let sub x seconds = of_float (x.unixtime -. seconds)

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

module Op =
struct
  let ( = ) a b = compare a b = 0
  let ( < ) a b = compare a b < 0
  let ( > ) a b = compare a b > 0
  let ( <= ) a b = compare a b <= 0
  let ( >= ) a b = compare a b >= 0
end

let tests = [
  "round milliseconds", test_round_milliseconds;
]
