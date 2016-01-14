(*
   General-purpose utilities not provided by Lwt_stream
*)

val iter_stream : int -> 'a Lwt_stream.t -> ('a -> unit Lwt.t) -> unit Lwt.t
  (*
     Parallel iteration over a stream.
     At most chunk_size (first parameter) items are processed at the same time.

     See also Util_conc.iter_stream, which returns the element out of order,
     but is faster if some items take longer to process than others.
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

val test_paged_stream : unit -> bool
val merge_lists : 'a list list -> 'a list
val test_merge : unit -> bool
val tests : (string * (unit -> bool)) list
