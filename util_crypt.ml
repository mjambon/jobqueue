(** Some utilities over cryptokit *)

open Batteries

(* Nonsensical keys, rotate as you see fit *)
let sign_args_key = "MasdjIFcn9193FLvc98764mCamCkasdf"
let mask_string_key = "POI09n28vnasASD09vasnNC87JnHG7od"

let base64_nonweb_encode s =
  Cryptokit.(transform_string (Base64.encode_compact_pad ()) s)

let base64_nonweb_decode s =
  Cryptokit.(transform_string (Base64.decode ()) s)

(** Base64 encode a string that is safe for the web
    as recommended by RFC 4648 https://tools.ietf.org/html/rfc4648
*)
let base64_web_encode s =
  String.map
    (function
      | '+' -> '-'
      | '/' -> '_'
      | c   -> c)
    (Cryptokit.(transform_string (Base64.encode_compact ()) s))

(** Base64 decode a string encoded by base64_web_encode *)
let base64_web_decode s =
  Cryptokit.(transform_string
               (Base64.decode ())
               (String.map
                  (function
                    | '-' -> '+'
                    | '_' -> '/'
                    | c    -> c)
                  s)
  )

 (** Generate a signature of a list of values *)
let sign_args ?(sep="/") args =
   (* Make a hash of all the values *)
   let hash = Cryptokit.MAC.hmac_sha1 sign_args_key in
   let text = String.concat sep (List.sort compare args) in
   base64_web_encode (Cryptokit.hash_string hash text)

(** Generate a hash of a text *)
let hash_text ?(hash=Cryptokit.Hash.sha1 ()) text =
  print_endline (Printf.sprintf "hash_text -> '%s'" text);
  base64_web_encode (Cryptokit.hash_string hash text)

(** Generate a hash of a list of values *)
let hash_args ?(sep="/") ?hash args =
  (* Make a hash of all the values *)
  hash_text ?hash (String.concat sep (List.sort compare args))


(** Check to see if the signature is correct for a list of args *)
let check_signed_args signature args = compare signature (sign_args args) = 0

(** Mask (encrypt) a string that can be unmasked via unmask_string *)
let mask_string s =
  base64_web_encode
    Cryptokit.(transform_string
                 (Cipher.aes ~pad:Padding.length mask_string_key Cipher.Encrypt)
                 s
    )

(** Unmask a string from mask_string *)
let unmask_string s =
  Cryptokit.(transform_string
               (Cipher.aes ~pad:Padding.length mask_string_key Cipher.Decrypt)
               (base64_web_decode s)
  )

let hmac_sha1 key input_string =
  Cryptokit.hash_string (Cryptokit.MAC.hmac_sha1 key) input_string

let sha1_digest_info h =
  "\x30\x21\x30\x09\x06\x05\x2b\x0e\x03\x02\x1a\x05\x00\x04\x14" ^ h;;

let pkcs1_pad rsa_key v =
  let tLen = String.length v in
  let emLen = rsa_key.Cryptokit.RSA.size / 8 in
  "\x00\x01" ^ String.make (emLen - tLen - 3) '\xff' ^ "\x00" ^ v

let rsa_sha1_hash text rsa_key =
  Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) text |>
  sha1_digest_info |>
  pkcs1_pad rsa_key |>
  Cryptokit.RSA.sign rsa_key |>
  base64_nonweb_encode

let check_rsa_sha1_hash text rsa_key signature =
  try
    let our_sig =
        Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) text |>
        sha1_digest_info |>
        pkcs1_pad rsa_key in
    let their_sig =
        base64_nonweb_decode signature |>
        Cryptokit.RSA.unwrap_signature rsa_key
    in
    our_sig = their_sig
  with _ -> false

module Test = struct
  open OUnit
  open Util_ounit

  let tests =
    "Util_crypt.Test" >::: [
      "base64_web_encode" >:: (fun () ->
        assert_string_equal ~msg:"~~~ý" "fn5-_Q" (base64_web_encode "~~~ý")
      );

      "base64_web_decode" >:: (fun () ->
        assert_string_equal ~msg:"fn5-_Q" "~~~ý" (base64_web_decode "fn5-_Q")
      );


      "sign_args" >:: (fun () ->
        let signature = sign_args ["foo"; "bar"; "baz"] in
        assert_bool "same args" (check_signed_args signature ["foo"; "bar"; "baz"]);
        assert_bool "different order" (check_signed_args signature ["bar"; "baz"; "foo"]);
        assert_bool "different args" (not (check_signed_args signature ["bar"; "baz"; "biz"]));
      );

      "mask_string" >:: (fun () ->
        let source = "The quick brown fox jumped over the lazy dog" in
        let masked = mask_string source in
        let unmasked = unmask_string masked in
        assert_string_equal ~msg:"unmasked" source unmasked;
        assert_bool "masked differs from source" (source <> masked);
      );
    ]
end
