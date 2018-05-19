(**
   Run CPU-intensive jobs in their own process and restrict
   how many such jobs may run in parallel and how long they take
   to complete.
*)

(** The type of a job queue. *)
type t

(** Return the number of jobs that have been submitted and haven't
    started yet. This can be used by the application to throttle job
    submission so as to avoid using too much memory.
*)
val pending : t -> int

(** Return the number of jobs currently running. *)
val running : t -> int

type 'a result = [
  (** Normal result of a job. *)
  | `Value of 'a

  (** Exception that was raised by the job.
      This exception is meant to be printed out only.
      The exception cannot be used in pattern-matching or in comparisons,
      due to implementation limitations having to do with serialization. *)
  | `User_exception of exn

  (** Indicates that the job timed out. *)
  | `Timeout

  (** Some other error occurred. *)
  | `Error of string
]

(** Extract the value or raise a [Failure] exception. *)
val value_exn : 'a result -> 'a

(**
   Create a queue of jobs to be run in their own process.

   @param max_running is the maximum number of jobs to run at the same
   time. The default is 1.
*)
val create : ?max_running:int -> unit -> t

(** Return the [max_running] parameter of a queue. *)
val max_running : t -> int

(**
   Run a computation in its own process using a call to [Unix.fork()].
   The result is serialized using [Marshal] and returned back to the
   parent using a pipe.

   Beware that any change in global variables won't be shared
   with the parent process.

   @param timeout the time limit for the execution of the job. There's
   no timeout by default.
*)
val submit : ?timeout:float -> t -> (unit -> 'a) -> 'a result Lwt.t

(**
   Parallel list map.
   [map q l f] applies [f] to each element of the list, each in a detached
   process.
*)
val map :
  ?timeout:float ->
  t ->
  'a list ->
  ('a -> 'b) ->
  'b result list Lwt.t
