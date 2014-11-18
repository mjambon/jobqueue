(*
  RFC 3339 compliant time with millisecond precision

  This is the modern standard for date and time.

  See Util_date for date only.
*)

(* date-time: "2012-11-13T17:33:31.192-08:00" *)
type t = private {
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

val format : fmt:string -> t -> string

val of_float : float -> t
  (* Format a date given in seconds since 1970-01-01 UTC *)

val to_float : t -> float
  (* Parse a date into seconds since 1970-01-01 UTC *)

val now : unit -> t

val add : t -> float -> t
  (* add seconds *)

val sub : t -> float -> t
  (* subtract seconds *)

val next : t -> t
  (* return a new date whose string representation is 1 ms later. *)

val compare : t -> t -> int
val min : t -> t -> t
val max : t -> t -> t

module Op :
sig
  val ( = ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
end

val tests : (string * (unit -> bool)) list
