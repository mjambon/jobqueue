(*
  RFC 3339 compliant time with millisecond precision

  This is the modern standard for date and time.

  See Util_date for date only.
*)

(* date-time: "2012-11-13T17:33:31.192-08:00" *)
type t = private {
  abstract_value: Util_abstract_value.t;
  unixtime : float;
  string : string;
}

val parse : string -> t option
  (* Parse a date and reformat it *)

val of_string : string -> t
val wrap : string -> t
  (* Parse a date and reformat it, raising an Invalid_argument exception if
     parsing fails. *)

val to_string : t -> string
val unwrap : t -> string
  (* can be achieved directly with coercion: (mydate :> string) *)

val of_float : float -> t
  (* Format a date given in seconds since 1970-01-01 UTC *)

val to_float : t -> float
  (* Parse a date into seconds since 1970-01-01 UTC *)

val now : unit -> t
val is_past : t -> bool
val is_future : t -> bool

val utc_start_of_week : t -> t
  (* return the beginning of the week according to UTC time *)

val utc_week : t -> int
  (* return the week identifier based on the calendar with UTC
     week boundaries. Week 0 is the week containing 1970-01-01. *)

val add : t -> float -> t
  (* add seconds *)

val sub : t -> float -> t
  (* subtract seconds *)

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


val next : t -> t
  (* return a new date whose string representation is 1 ms later. *)

val compare : t -> t -> int

val ( = ) : t -> t -> bool
val ( < ) : t -> t -> bool
val ( > ) : t -> t -> bool
val ( <= ) : t -> t -> bool
val ( >= ) : t -> t -> bool

val min : t -> t -> t
val max : t -> t -> t

(* Meant to be used as `let open Util_time.Op in` *)
module Op : sig
  val ( = ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool

  val min : t -> t -> t
  val max : t -> t -> t
end

val hour_of_day : int -> t -> t
  (* return a new date with the time set to the specified hour *)

val diff_seconds : t -> t -> float
  (* return difference in seconds between two timestamps *)

val diff_days : t -> t -> float
  (* return difference in days between two timestamps *)

(*
   Same type, exported rounded down to the second
   and serialized into plain decimal notation
   e.g. "1424829100"

   Note that of_float and of_string do not perform any rounding.
*)
module As_unixtime : sig
  type time = t
  type t = time
  val of_float : float -> t
  val to_float : t -> float (* rounded *)
  val wrap : float -> t (* same as of_float *)
  val unwrap : t -> float (* same as to_float *)
  val of_string : string -> t
  val to_string : t -> string (* rounded *)
end

val tests : (string * (unit -> bool)) list
