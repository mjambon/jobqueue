val default_conc : int
  (** The default value for the [conc] parameter,
      which is the maximum number of elements processed concurrently.
      This value is 20.
  *)

(** {1} Concurrent iterations over streams *)

val map_stream :
  ?conc: int ->
  'a Lwt_stream.t ->
  ('a -> (bool * 'b) Lwt.t) ->
  (bool * 'b) Lwt_stream.t
  (**
     [map_stream ~conc strm f] converts each element of a stream,
     [conc] at a time. Order is not preserved.

     Exceptions are handled with [Lwt.async_exception_hook] and do not
     interrupt the output stream.
     If [f] returns [false], it breaks the output stream as soon as all pending
     jobs have returned.
  *)

val iter_stream :
  ?conc: int ->
  'a Lwt_stream.t ->
  ('a -> bool Lwt.t) ->
  bool Lwt.t
  (** Same as [map_stream] but no output stream is produced.
      The result indicates whether the input stream was processed entirely. *)

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
