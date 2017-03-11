
val pop : 'a Stream.t -> 'a option
(* Consume the first element and return it. *)

val stream_of_channel : in_channel -> string Stream.t
val stream_of_file : string -> string Stream.t * (unit -> unit)
(* Make a stream of the lines of a channel or file. *)

val iteri : (int -> 'a -> unit) -> 'a Stream.t -> unit
(* Stream.iter with item position. *)

val map : ('a -> 'b) -> 'a Stream.t -> 'b Stream.t
val mapi : (int -> 'a -> 'b) -> 'a Stream.t -> 'b Stream.t
(* 1:1 mapping. *)

val filter : ('a -> bool) -> 'a Stream.t -> 'a Stream.t
(* Filtering *)

val select : ('a -> 'b option) -> 'a Stream.t -> 'b Stream.t
(* map + filter *)

val skip_to : ('a -> bool) -> 'a Stream.t -> 'a Stream.t
(* Discard lazily non-matching elements of a stream until
   one matching element is found.
*)

val really_skip_to :
  ?discard: ('a -> unit) ->
  ('a -> bool) -> 'a Stream.t -> int
(* Discard immediately non-matching elements of a stream until
   one matching element is found.
   Returns the number of skipped elements.
*)


val flatten : ('a -> 'b list) -> 'a Stream.t -> 'b Stream.t
(* Map each element to a list and add to a single stream. *)


val concat : 'a Stream.t list -> 'a Stream.t
(* Concatenate a list of streams. *)


val reduce :
  ('a -> 'a -> int) ->
  ('a -> 'b list -> 'c) -> ('a * 'b) Stream.t -> 'c Stream.t
(* Group by keys within a sorted list. *)


val merge :
  ('k -> 'k -> int) ->
  ('k -> 'v2) ->
  ('v2 -> 'v1 -> 'v2) -> ('k * 'v1) Stream.t list -> ('k * 'v2) Stream.t
(* [merge cmp init fold l]: Merge streams of items sorted by keys. *)
