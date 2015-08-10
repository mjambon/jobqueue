(** Hex encoder/decoder *)

(* from netclient/http_client.ml *)
let hex_digits_lower = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
                          '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f' |]

let hex_digits_upper = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
                          '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |]

let encode ?(upper=false) s =
  let hex_digits = if upper then hex_digits_upper else hex_digits_lower in
  let l = String.length s in
  let t = Bytes.make (2*l) ' ' in
  for n = 0 to l - 1 do
    let x = Char.code s.[n] in
    Bytes.set t (2*n) hex_digits.( x lsr 4 );
    Bytes.set t (2*n+1) hex_digits.( x land 15 );
  done;
  Bytes.to_string t

exception Malformed

let decode_char c =
  match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'A'..'F' -> Char.code c - Char.code 'A' + 10
    | 'a'..'f' -> Char.code c - Char.code 'a' + 10
    | _        -> raise Malformed

let error msg s =
  invalid_arg (
    Printf.sprintf "Hex.decode, %s: %s"
      msg (if String.length s < 40 then s else String.sub s 0 40 ^ " ...")
  )

(* this assumes no extra characters/spacing between hex digits *)
let decode s =
  let len = String.length s in
  if len mod 2 = 1 then
    error "odd length" s;
  let t_len = len / 2 in
  let t = Bytes.make t_len ' ' in
  try
    for n = 0 to t_len - 1 do
      let high = decode_char s.[2*n] in
      let low = decode_char s.[2*n+1] in
      let code = (high lsl 4) lor low in
      Bytes.set t n (Char.chr code)
    done;
    Bytes.to_string t
  with Malformed ->
    error "invalid input" s
