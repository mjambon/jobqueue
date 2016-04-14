(*
   General-purpose utilities not provided by Lwt_stream
*)

val iter : int -> 'a Lwt_stream.t -> ('a -> unit Lwt.t) -> unit Lwt.t
  (*
     Parallel iteration over a stream.
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
  *)

val merge :
  ?cmp:('k -> 'k -> int) ->
  get_key:('v -> 'k) ->
  'v Lwt_stream.t list -> 'v Lwt_stream.t
  (*
     Merge ordered streams into one.
  *)

(* Testing *)

val test_iter : unit -> bool
val test_paged_stream : unit -> bool
val merge_lists : 'a list list -> 'a list
val test_merge : unit -> bool
val tests : (string * (unit -> bool)) list
