exception Shutdown

val is_shutting_down : unit -> bool

val shutdown : float -> unit
  (*
     Signal all watcher threads that the system is shutting down,
     wait for the given duration (seconds),
     then exit the process.

     Works only from within the lwt main loop.
  *)

val shutdown_on_sigterm : float -> unit
  (*
     Trigger a shutdown when the TERM signal is received.
  *)

val wait_for_shutdown : unit -> unit Lwt.t
  (*
     Create an lwt thread that returns when the system starts shutting down;
     returns immediately if the system has already started shutting down.
  *)

val cancel_on_shutdown : 'a Lwt.t -> 'a Lwt.t
  (*
     Terminate the given lwt thread with the Lwt.Canceled exception
     when system is shutting down.
  *)
