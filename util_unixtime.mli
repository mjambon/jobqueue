(*
   Same as Util_time, but:
   - use 1-second precision
   - serialize to/from decimal integers representing the number of seconds
     since January 1, 1970, as returned by Unix.time().

   The main advantage of this representation over RFC-3339 (Util_time)
   is that the serialized representation can be used as stable keys.
*)

type t

val equal : t -> t -> bool
val compare : t -> t -> int

val to_string : t -> string
val of_string : string -> t

val of_float : float -> t
val to_float : t -> float

val wrap : float -> t
val unwrap : t -> float

val to_time : t -> Util_time.t
val of_time : Util_time.t -> t

val now : unit -> t

val add : t -> float -> t
val sub : t -> float -> t
  (* Add or subtract seconds *)

val add_day : t -> float -> t
val sub_day : t -> float -> t
  (* Add or subtract days *)

val add_hour : t -> float -> t
val sub_hour : t -> float -> t
  (* Add or subtract hours *)

val add_min : t -> float -> t
val sub_min : t -> float -> t
  (* Add or subtract minutes *)

val set_sec : t -> int -> t
  (*
    Set seconds past the minute

    For example:
    The float 1424225951. representing the time 2015-02-17T18:19:11

    (set_sec 1424225951. 59) would yield 1424225999. which represents
    the time 2015-02-17T18:19:59
  *)
val set_min : t -> int -> t
  (* set minutes past the hour *)
val set_hour : t -> int -> t
  (* set hours into the day *)

val tests : (string * (unit -> bool)) list
