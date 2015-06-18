(** Concurrent processing of streams and lists of lwt jobs,
    with a limit on how many jobs can run concurrently.

    Note that exceptions are wrapped within [Util_exn.Traced] so as to
    preserve the stack trace where the exception was caught.
    Catching and inspecting exceptions raised during a [Util_conc] iteration
    is not recommended, but if you need to do it, you'll have to
    unwrap the exception using for example [Util_exn.unwrap_traced].
*)

val default_conc : int
  (** The default value for the [conc] parameter,
      which is the maximum number of elements processed concurrently.
      This value is 20.
  *)

(** {1} Concurrent iterations over lists, processing at most N elements
    at a time
*)

val map : ?conc: int -> 'a list -> ('a -> 'b Lwt.t) -> 'b list Lwt.t
  (** Same as [map_stream] but over a list.
      Unlike [map_stream], the output list is reordered to match
      the order of the input list.
      Unlike [map_stream], the first exception encountered chronologically
      is propagated into the result thread. *)

val iter : ?conc: int -> 'a list -> ('a -> unit Lwt.t) -> unit Lwt.t
  (** Same as [iter] but doesn't build an output list. *)

val filter_map :
  ?conc: int ->
  'a list ->
  ('a -> 'b option Lwt.t) ->
  'b list Lwt.t
  (** Analog to BatList.filter_map *)

val filter : ?conc: int -> 'a list -> ('a -> bool Lwt.t) -> 'a list Lwt.t
  (** Analog to BatList.filter *)

val exists : ?conc: int -> 'a list -> ('a -> bool Lwt.t) -> bool Lwt.t

val for_all : ?conc: int -> 'a list -> ('a -> bool Lwt.t) -> bool Lwt.t


(**/**)
val tests : (string * (unit -> bool)) list
