open Printf

exception Traced of exn * string
  (* An exception with its stack trace *)

let plural n =
  if abs n >= 2 then "s"
  else ""

let print_omitting buf n =
  bprintf buf "... (omitting %i repeated line%s)\n" n (plural n)

(*
   Hash table that doesn't exceptions
   (based on the current implementation in hashtbl.ml)
*)
module H = Hashtbl.Make (
struct
  type t = int
  let equal i j = i = j
  let hash i = i land max_int
end)

(*
   Accumulate unique lines into a buffer,
   without raising an exception because it would disturb the stack trace.
*)
let rec scan_lines tbl buf s pos ellipsis =
  let len = String.length s in
  let h = ref 0 in
  let pos2 = ref pos in
  let continue = ref true in
  while !continue do
    let i = !pos2 in
    if i <= len - 1 then (
      (match s.[i] with
       | '\n' -> continue := false
       | c -> h := 223 * !h + (Char.code c)
      );
      incr pos2
    )
    else
      continue := false;
  done;
  let ellipsis2 =
    if H.mem tbl !h then
      ellipsis + 1
    else (
      H.add tbl !h ();
      if ellipsis > 0 then
        print_omitting buf ellipsis;
      Buffer.add_substring buf s pos (!pos2 - pos);
      0
    )
  in
  if !pos2 < len then
    scan_lines tbl buf s !pos2 ellipsis2
  else
    if ellipsis2 > 0 then
      print_omitting buf ellipsis2


(* Replace any sequence of lines that already occurred by "...\n" *)
let make_compact s =
  let tbl = H.create 20 in
  let buf = Buffer.create 1000 in
  scan_lines tbl buf s 0 0;
  Buffer.contents buf

let test_compact_trace () =
  assert (make_compact "" = "");
  assert (make_compact "a\nb" = "a\nb");
  assert (make_compact "a\nb\n" = "a\nb\n");
  assert (make_compact "a\na" = "a\n... (omitting 1 repeated line)\n");
  assert (make_compact "a\na\na" = "a\n... (omitting 2 repeated lines)\n");
  assert (make_compact "x\na\nbc\na\nbc\nx"
          = "x\na\nbc\n... (omitting 3 repeated lines)\n");
  assert (make_compact "x\na\nbc\na\nbc\nx\ny"
          = "x\na\nbc\n... (omitting 3 repeated lines)\ny");
  true

let custom_exception_printer = function
  | Unix.Unix_error (kind, func, arg) ->
      Some (sprintf "Unix_error(%s, %S, %S)"
              (Unix.error_message kind) func arg)
  | _ -> None

let () = Printexc.register_printer custom_exception_printer

let get_compact_backtrace () =
  let backtrace = Printexc.get_backtrace () in
  make_compact backtrace

let rec string_of_exn ?(earlier_trace = "") e =
  (*
    When using netsys (from Ocamlnet), some bug causes the backtrace
    to be incorrect if Printexc.to_string is called first.
    Therefore it is important to call Printexc.get_backtrace first.
  *)
  match e with
  | Traced (e0, trace0) ->
      string_of_exn ~earlier_trace: (earlier_trace ^ trace0) e0
  | e ->
      let backtrace = get_compact_backtrace () in
      let s = Printexc.to_string e in
      s ^ "\n" ^ earlier_trace ^ backtrace

let constructor (e : exn) : string =
  let r = Obj.repr e in
  assert (Obj.is_block r);
  assert (Obj.size r >= 1);
  let boxed_string = Obj.field r 0 in
  assert (Obj.is_block boxed_string);
  assert (Obj.size boxed_string = 1);
  let string = Obj.field boxed_string 0 in
  assert (Obj.tag string = Obj.string_tag);
  Obj.obj string

let rec trace ?(earlier_trace = "") e =
  match e with
  | Traced (e0, trace0) ->
      trace ~earlier_trace: (earlier_trace ^ trace0) e0
  | e ->
      let backtrace = get_compact_backtrace () in
      constructor e ^ "\n" ^ earlier_trace ^ backtrace

let make_traced e =
  Traced (e, get_compact_backtrace ())

let rec unwrap_traced = function
  | Traced (e, t) -> unwrap_traced e
  | e -> e

let trace_hash e =
  String.sub (Digest.to_hex (Digest.string (trace e))) 0 8

let tests = [
  "compact trace", test_compact_trace;
]
