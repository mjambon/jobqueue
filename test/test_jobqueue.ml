(*
   Test suite for the Jobqueue module/library.
*)

open Lwt

let test_basics () =
  let pool = Jobqueue.create_pool ~max_running:10 in
  let parent_pid = Unix.getpid () in
  Lwt_main.run (
    Jobqueue.submit ~pool ~timeout:1. (fun () ->
      Unix.getpid ()
    )
    >>= function
    | `Value child_pid ->
        assert (Unix.getpid () = parent_pid);
        assert (child_pid <> parent_pid);
        return ()
    | _ ->
        assert false
  )

let jobqueue_suite = [
  "basics", `Quick, test_basics;
]

let suites = [
  "Jobqueue", jobqueue_suite;
]

let () = Alcotest.run "proj" suites
