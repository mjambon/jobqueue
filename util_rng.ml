open Cryptokit.Random

let make_rng () =
  pseudo_rng
    (string_of_int (Unix.getpid ()) ^ (string (device_rng "/dev/urandom") 20) )

let get_the_rng =
  let pid = ref (Unix.getpid ()) in
  let the_rng = ref (make_rng ()) in
  fun () ->
    if !pid <> Unix.getpid () then (
      pid := Unix.getpid ();
      the_rng := make_rng ();
    );
    !the_rng


let rec int64_of_bytes n i bytes =
  if 0 > i then n
  else
    int64_of_bytes (Int64.add (Int64.shift_left n 8)
                              (Int64.of_int (int_of_char bytes.[i])))
                   (i - 1)
                   bytes

let string len = string ( get_the_rng () ) len

let hex len =
  Util_hex.encode (string len)

let random bound =
  let bytes = string 7 in
  let n = int64_of_bytes 0L 6 bytes in
  Int64.(to_int (rem n (of_int bound)))

let uuid () =
  let x len = Util_hex.encode (string len) in
  let x3 () = Printf.sprintf "%03x" (random 0xfff) in
  let y =
    match (random 4) with
      | 0 -> '8'
      | 1 -> '9'
      | 2 -> 'a'
      | 3 -> 'b'
      | _ -> assert false in

  Printf.sprintf "%s-%s-4%s-%c%s-%s"
    (x   4)
    (x   2)
    (x3 ())
     y
    (x3 ())
    (x   6)
