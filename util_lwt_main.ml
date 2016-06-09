(*
   Substitute for the lwt event loop (Util_lwt_main.run),
   which catches the TERM signal and gives 5 seconds
   for the threads to finish what they were doing,
   most importantly release external locks.
*)
let run t =
  Util_shutdown.shutdown_on_sigterm 5.;
  Lwt_main.run (Util_shutdown.cancel_on_shutdown t)

(*
   Run an infinite loop with graceful shutdown like `run` above.
*)
let loop () =
  run (
    (* create forever-sleeping thread *)
    fst (Lwt.wait ())
  )
