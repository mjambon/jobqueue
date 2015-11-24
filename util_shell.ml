open Printf
open Lwt

(*
   Quote an arbitrary string for safe use in a sh/bash command

   ' -> \'
   xx -> 'xx'
*)
let quote =
  let single_quote = Pcre.regexp "'" in
  fun s ->
    let safe_fragments = Pcre.split ~rex:single_quote s in
    let quoted_fragments =
      BatList.map
        (function
          | "" -> ""
          | s -> sprintf "'%s'" s)
        safe_fragments
    in
    String.concat "\\'" quoted_fragments

let test_quote () =
  let check input expected_output =
    quote input = expected_output
  in
  assert (check "don't\nworry" "'don'\\''t\nworry'");
  assert (check "$HOME" "'$HOME'");
  true

(*
   Return the exit status of a Bourne shell command,
   or fail with an exception if a signal kills or stops the process.
*)
let sys_command cmd =
  Lwt_process.exec (Lwt_process.shell cmd) >>= function
  | Unix.WEXITED n -> return n
  | Unix.WSIGNALED n -> failwith (sprintf "Command killed by signal %i" n)
  | Unix.WSTOPPED n -> failwith (sprintf "Command stopped by signal %i" n)

let tests = [
  "quote", test_quote;
]
