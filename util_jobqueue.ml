(*
   Queue of jobs run within their own process with cpu time limit.
*)

open Printf
open Lwt

type 'a result = [
  | `Value of 'a
  | `Capacity_exceeded
  | `Timeout
  | `User_exception of exn
      (* Exception raised by the user's function.
         It went through marshalling, so pattern matching on it won't work. *)
  | `Error of string
      (* Error internal to the implementation of this module. *)
]

let run_child fd_out f =
  let result : _ result =
    try `Value (f ())
    with e -> `User_exception e
  in
  let oc = Unix.out_channel_of_descr fd_out in
  Marshal.to_channel oc result [];
  close_out oc

(*
   Run an lwt thread with a timeout.
   If the timeout is reached, None is returned and it is the responsibility
   for the caller to kill the job that's still running in the background.
*)
let with_timeout ~timeout x =
  let sleep =
    Lwt_unix.sleep timeout >>= fun () ->
    return `Timeout
  in
  let job =
    x >>= fun result ->
    return result
  in
  Lwt.choose [ sleep; job ]

let rec waitpid pid =
  match
    try Some (Lwt_unix.waitpid [] pid)
    with Unix.Unix_error (Unix.EINTR, _, _) -> None
  with
  | Some result -> result
  | None -> waitpid pid

let terminate_child pid =
  Unix.kill pid Sys.sigkill

let run_parent ~timeout ic child_pid =
  with_timeout ~timeout (Lwt_io.read_value ic) >>= fun result ->
  Lwt_io.close ic >>= fun () ->
  (match result with
   | `Timeout -> terminate_child child_pid
   | `Value _
   | `User_exception _ -> ()
   | `Capacity_exceeded
   | `Error _ -> assert false
  );
  waitpid child_pid >>= fun (pid, process_status) ->
  match process_status with
  | Unix.WEXITED 0 ->
      return result
  | Unix.WSIGNALED n when n = Sys.sigkill ->
      return result

  | Unix.WEXITED n ->
      return (`Error (sprintf "Child process exited with code %i" n))
  | Unix.WSIGNALED n ->
      return (`Error (sprintf "Child process killed by signal %i" n))
  | Unix.WSTOPPED n ->
      return (`Error (sprintf "Child process stopped by signal %i" n))

let run_job ~timeout (f : unit -> 'a) : 'a result Lwt.t =
  Lwt_io.flush_all () >>= fun () ->
  let fd_read_from_child, fd_write_to_parent = Unix.pipe () in
  let child_pid = Lwt_unix.fork () in
  if child_pid = 0 then (
    (* Child process *)
    try
      (* Everything is done synchronously in the child from here.
         Not sure how to deal with Lwt's main loop that's still running. *)
      Unix.close fd_read_from_child;
      run_child fd_write_to_parent f;
      exit 0
    with e ->
      eprintf "Uncaught exception in Util_jobqueue: %s\n%!"
        (Printexc.to_string e);
      exit 1
  )
  else (
    (* Parent process *)
    let fd_read_from_child =
      Lwt_io.of_unix_fd ~mode:Lwt_io.input fd_read_from_child in
    let fd_write_to_parent =
      Lwt_io.of_unix_fd ~mode:Lwt_io.output fd_write_to_parent in
    Lwt_io.close fd_write_to_parent >>= fun () ->
    run_parent ~timeout fd_read_from_child child_pid
  )

type pool = {
  mutable running : int;
  max_running : int;
}

let create_pool ~max_running =
  { running = 0;
    max_running }

let submit ~pool ~timeout f : _ result Lwt.t =
  assert (pool.running >= 0);
  if pool.running >= pool.max_running then
    return `Capacity_exceeded
  else (
    pool.running <- pool.running + 1;
    Lwt.finalize
      (fun () -> run_job ~timeout f)
      (fun () ->
         pool.running <- pool.running - 1;
         return ()
      )
  )

(* Tests *)

let sleep t = ignore (Unix.select [] [] [] t)

let test_capacity () =
  let main () =
    let pool = create_pool 2 in
    let job () = () in
    Lwt_list.map_p
      (fun () -> submit ~pool ~timeout:1. job)
      [(); (); ()]
    >>= fun result ->
    let expected = [ `Value (); `Value (); `Capacity_exceeded; ] in
    assert (List.sort compare result = List.sort compare expected);
    return true
  in
  Lwt_main.run (main ())

let test_timeout () =
  let main () =
    let pool = create_pool 5 in
    let job () = sleep 0.11 in
    submit ~pool ~timeout:0.1 job >>= function
    | `Timeout -> return true
    | `Value _ -> assert false
    | `Capacity_exceeded -> assert false
    | `User_exception _ -> assert false
    | `Error s ->
        eprintf "Error: %s\n%!" s;
        assert false
  in
  Lwt_main.run (main ())

let test_values () =
  let main () =
    let pool = create_pool 5 in
    let job () =
      Array.init 3 (fun i -> i)
    in
    submit ~pool ~timeout:0.1 job >>= function
    | `Value [| 0; 1; 2 |] -> return true
    | `Value _ -> assert false
    | `Capacity_exceeded -> assert false
    | `Timeout -> assert false
    | `User_exception _ -> assert false
    | `Error _ -> assert false
  in
  Lwt_main.run (main ())

let test_exceptions () =
  let main () =
    let pool = create_pool 5 in
    let job () = failwith "this is a test" in
    submit ~pool ~timeout:1. job >>= function
    | `User_exception _ -> return true
    | `Value _ -> assert false
    | `Capacity_exceeded -> assert false
    | `Timeout -> assert false
    | `Error _ -> assert false
  in
  Lwt_main.run (main ())

let tests = [
  "capacity", test_capacity;
  "timeout", test_timeout;
  "values", test_values;
  "exceptions", test_exceptions;
]
