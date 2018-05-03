(*
   Run CPU-intensive jobs in their own process and restrict
   how many such jobs may run in parallel
   and how long they take to complete.
*)

type pool

type 'a result = [
  | `Value of 'a
  | `Capacity_exceeded
  | `Timeout
  | `User_exception of exn
  | `Error of string
]

val create_pool : max_running:int -> pool
  (*
     All submitted jobs can run at the same time, sharing resources.
     We keep track of how many jobs run in parallel and reject new jobs
     if too many jobs are already running.
     It is simple but not optimal if the maximum number of jobs exceeds the
     number of CPUs.
  *)

val submit : pool:pool -> timeout:float -> (unit -> 'a) -> 'a result Lwt.t
  (*
     Run a computation in its own process.

     If too many jobs are already running or if the job takes longer than
     the specified timeout, the result is None.

     Beware that any change in global variables won't be shared
     with the parent process.
  *)

val tests : (string * (unit -> bool)) list
