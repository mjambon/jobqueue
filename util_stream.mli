(*
   Stream utilities
*)

val fold_left : ('acc -> 'elt -> 'acc) -> 'acc -> 'elt Stream.t -> 'acc
(* Scan and accumulate.
   Same as List.fold_left, but on a stream. *)

val to_list : 'a Stream.t -> 'a list
(* Turn a stream into a list *)

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
  ('a -> 'a -> int) ->
  'a Stream.t list -> 'a Stream.t
(*
   Merge sorted streams into one.
*)

val merge_full :
  ('k -> 'k -> int) ->
  ('k -> 'local_acc) ->
  ('local_acc -> 'v -> 'local_acc) ->
  ('k * 'v) Stream.t list -> ('k * 'local_acc) Stream.t
(*
   [merge_full cmp init fold l]: Merge streams of items sorted by keys.

   For each new key, we create an accumulator and we add to it all the values
   that have this key. This can be used to deduplicate values or to
   summarize the values found for each key, without loading them
   all at once in memory.
*)

val tests : (string * (unit -> bool)) list
val test_merge : unit -> bool
val test_merge_full : unit -> bool
