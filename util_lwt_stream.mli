(*
   General-purpose utilities not provided by Lwt_stream
*)

val iter : int -> 'a Lwt_stream.t -> ('a -> unit Lwt.t) -> unit Lwt.t
  (*
     Parallel iteration over a stream.
     The elements of the input stream are computed sequentially, but the
     user-provided function applied to each element runs concurrently.
     The first parameter max_threads indicates how many items
     will be processed concurrently.
  *)

val create_paged_stream:
  'acc -> ('acc -> ('acc * 'elem list * bool) Lwt.t) -> 'elem Lwt_stream.t
  (*
     Create a stream of items from a function that
     fetches pages of items, using an accumulator
     to carry information such as page number.
     The boolean that it returns indicates whether a next page
     must be fetched.

     Errors should be represented as elements e.g.,

       type elem = Elt of t | Error of error

  *)

val merge :
  ?cmp:('k -> 'k -> int) ->
  ?exn_handler:(exn -> 'v option Lwt.t) ->
  get_key:('v -> 'k) ->
  'v Lwt_stream.t list -> 'v Lwt_stream.t
  (*
     Merge ordered streams into one.

     The exception handler may raise an exception, return some value,
     or interrupt the stream without an error by returning None.
  *)

(* Testing *)

val test_iter : unit -> bool
val test_paged_stream : unit -> bool
val merge_lists : 'a list list -> 'a list
val test_merge : unit -> bool
val tests : (string * (unit -> bool)) list
