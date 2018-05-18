(*
   Queue of jobs run within their own process with cpu time limit.
*)

open Printf
open Lwt

type 'a result = [
  | `Value of 'a
  | `Timeout
  | `User_exception of exn
      (* Exception raised by the user's function.
         It went through marshalling, so pattern matching on it won't work. *)
  | `Error of string
      (* Error internal to the implementation of this module. *)
]

let value_exn = function
  | `Value x -> x
  | `Timeout ->
      failwith "Jobqueue timeout"
  | `User_exception e ->
      failwith (sprintf "Jobqueue user exception: %s"
                  (Printexc.to_string e))
  | `Error s ->
      failwith (sprintf "Jobqueue error: %s" s)

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
  Lwt.choose [ sleep; x ]

let with_opt_timeout opt_timeout x =
  match opt_timeout with
  | None -> x
  | Some timeout -> with_timeout ~timeout x

let rec waitpid pid =
  match
    try Some (Lwt_unix.waitpid [] pid)
    with Unix.Unix_error (Unix.EINTR, _, _) -> None
  with
  | Some result -> result
  | None -> waitpid pid

let terminate_child pid =
  Unix.kill pid Sys.sigkill

let run_parent opt_timeout ic child_pid =
  with_opt_timeout opt_timeout (Lwt_io.read_value ic) >>= fun result ->
  Lwt_io.close ic >>= fun () ->
  (match result with
   | `Timeout -> terminate_child child_pid
   | `Value _
   | `User_exception _ -> ()
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

let run_job opt_timeout (f : unit -> 'a) : 'a result Lwt.t =
  Lwt_io.flush_all () >>= fun () ->
  let fd_read_from_child, fd_write_to_parent = Lwt_unix.pipe_in () in
  let child_pid = Lwt_unix.fork () in
  if child_pid = 0 then (
    (* Child process *)
    try
      (* Everything is done synchronously in the child from here.
         Not sure how to deal with Lwt's main loop that's still running. *)
      ignore (Lwt_unix.close fd_read_from_child); (* does this work? *)
      run_child fd_write_to_parent f;
      exit 0
    with e ->
      eprintf "Fatal exception in child process created by Jobqueue: %s\n%!"
        (Printexc.to_string e);
      exit 1
  )
  else (
    (* Parent process *)
    let fd_read_from_child =
      Lwt_io.of_fd ~mode:Lwt_io.input fd_read_from_child in
    Unix.close fd_write_to_parent;
    run_parent opt_timeout fd_read_from_child child_pid
  )

type t = {
  mutable pending : int; (* informational only *)
  mutable running : int;
  avail_condition : unit Lwt_condition.t;
  max_running : int;
}

let pending q =
  q.pending

let running q =
  q.running

let create ?(max_running = 1) () =
  if max_running <= 0 then
    invalid_arg "Jobqueue.create"
  else {
    pending = 0;
    running = 0;
    avail_condition = Lwt_condition.create ();
    max_running;
  }

let submit ?timeout:opt_timeout queue f : _ result Lwt.t =
  assert (queue.pending >= 0);
  assert (queue.running >= 0);
  let run () =
    queue.pending <- queue.pending - 1;
    queue.running <- queue.running + 1;
    Lwt.finalize
      (fun () -> run_job opt_timeout f)
      (fun () ->
         queue.running <- queue.running - 1;
         Lwt_condition.signal queue.avail_condition ();
         return ()
      )
  in
  queue.pending <- queue.pending + 1;
  if queue.running >= queue.max_running then
    Lwt_condition.wait queue.avail_condition >>= fun () ->
    run ()
  else
    run ()

let map ?timeout queue l f =
  let promises =
    List.fold_left (fun acc x ->
      let job () = f x in
      submit ?timeout queue job :: acc
    ) [] l
    |> List.rev
  in
  Lwt_list.map_s (fun x -> x) promises
