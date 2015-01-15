open Printf

type t = float

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
