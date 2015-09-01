open Printf

let custom_exception_printer = function
  | Unix.Unix_error (kind, func, arg) ->
      Some (sprintf "Unix_error(%s, %S, %S)"
              (Unix.error_message kind) func arg)
  | _ -> None

let () = Printexc.register_printer custom_exception_printer

let trace_hash e =
  let s = Printexc.exn_slot_name e ^ "\n" ^ Trax.get_trace e in
  String.sub (Digest.to_hex (Digest.string s)) 0 8
