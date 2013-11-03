open Printf

let plural n =
  if abs n >= 2 then "s"
  else ""

let print_omitting buf n =
  bprintf buf "... (omitting %i line%s)\n" n (plural n)

let rec scan_lines tbl buf s pos ellipsis =
  let len = String.length s in
  let h = ref 0 in
  let pos2 = ref pos in
  (try
     for i = pos to len - 1 do
       match s.[i] with
           '\n' ->
             pos2 := i + 1;
             raise Exit
         | c -> h := 223 * !h + (Char.code c)
     done;
     pos2 := len
   with Exit -> ());
  let ellipsis2 =
    if Hashtbl.mem tbl !h then
      ellipsis + 1
    else (
      Hashtbl.add tbl !h ();
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
  let tbl = Hashtbl.create 20 in
  let buf = Buffer.create 1000 in
  scan_lines tbl buf s 0 0;
  Buffer.contents buf

let custom_exception_printer = function
  | Unix.Unix_error (kind, func, arg) ->
      Some (sprintf "Unix_error(%s, %S, %S)"
              (Unix.error_message kind) func arg)
  | _ -> None

let string_of_exn e =
  (*
    When using netsys (from Ocamlnet), some bug causes the backtrace
    to be incorrect if Printexc.to_string is called first.
    Therefore it is important to call Printexc.get_backtrace first.
  *)
  let backtrace = Printexc.get_backtrace () in
  let s =
    match custom_exception_printer e with
        Some s -> s
      | None -> Printexc.to_string e
  in
  s ^ "\n" ^ make_compact backtrace
