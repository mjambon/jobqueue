type t = {
  unixtime : float;
  string : string;
}

let of_float t = {
  unixtime = t;
  string = Nldate.mk_internet_date ~localzone:true ~digits:3 t;
}

let to_float x = x.unixtime

let parse s =
  try Some (of_float (Nldate.since_epoch_approx (Nldate.parse s)))
  with _ -> None

let of_string s =
  match parse s with
      None -> invalid_arg ("Util_time.of_string_exn: " ^ s)
    | Some x -> x

let to_string x = x.string

let now () = of_float (Unix.gettimeofday ())

let add x seconds = of_float (x.unixtime +. seconds)
let sub x seconds = of_float (x.unixtime -. seconds)

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
