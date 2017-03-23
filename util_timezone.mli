(*
   Timezones and conversion between universal time and local times.
*)

type t = private string
  (* timezone name, e.g. "America/Los_Angeles" *)

val is_supported : t -> bool
  (* whether the timezone is in our list *)

val of_string : string -> t
  (* check that the timezone is supported and return the same string
     or fail with an exception. *)

val to_string : t -> string

val wrap : string -> t
val unwrap : t -> string

val fallback_timezone : t

val with_timezone : t -> (unit -> 'a) -> 'a
  (* execute given function with the environment variable TZ set to the
     specified value.

     Not thread-safe.
  *)

val localtime : t -> float -> Unix.tm
  (* Same as Unix.localtime, using the specified timezone instead
     of the system's default. *)

val mktime : t -> Unix.tm -> float * Unix.tm
  (*
     Same as Unix.mktime, but operates within the specified timezone
     instead of the system's default.

     Specifying the proper timezone is known to correct:
     - the float returned (seconds since epoch)
     - the `tm_isdt` field
     - times that occur during a leap forward according to one timezone or
       and not the other
  *)

val timezones : t list
  (* List of all timezones in a US-friendly order.
     May contain duplicates.
  *)

val timezone_mapping : (t * string) list
  (* Nicknames for common US time zones *)

val display_name : t -> string
  (* Try to use a time zone's nickname, otherwise keep it as is. *)

(* Some commonly-used timezones *)
val utc : t
val america_los_angeles : t
