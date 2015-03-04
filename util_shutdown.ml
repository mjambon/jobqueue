open Lwt

let shutdown_condition = Lwt_condition.create ()
let shutting_down = ref false

let is_shutting_down () = !shutting_down

let shutdown grace_period =
  if not !shutting_down then (
    shutting_down := true;
    Lwt_condition.signal shutdown_condition ();
    (* Give other threads some time to finish what they were doing *)
    Printf.printf "SHUTDOWN in %g seconds\n%!" grace_period;
    ignore (
      Lwt_unix.sleep grace_period >>= fun () ->
      Printf.printf "STOP\n%!";
      exit 0
    )
  )

let shutdown_on_sigterm grace_period =
  let _handler_id =
    Lwt_unix.on_signal Sys.sigterm (fun signum -> shutdown grace_period) in
  ()

let wait_for_shutdown () =
  if is_shutting_down () then
    return ()
  else
    Lwt_condition.wait shutdown_condition

exception Shutdown

let cancel_on_shutdown x =
  ignore
    (pick [ (x >>= fun y -> return ());
            wait_for_shutdown () ]
    );
  x
