(*
   Utilities having to do with JSON processing, and missing
   from yojson and atdgen.
*)

let string_of_json_string json =
  match Yojson.Basic.from_string json with
  | `String s -> s
  | _ -> invalid_arg "Util_json.string_of_json_string"
