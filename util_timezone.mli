(*
   Timezones and conversion between universal time and local times.
*)

type t = string
  (* timezone name, e.g. "America/Los_Angeles" *)

val is_supported : t -> bool
  (* whether the timezone is in our list *)

val timezone_of_string : string -> t
  (* check that the timezone is supported and return the same string
     or fail with an exception. *)

val fallback_timezone : t

val with_timezone : t -> (unit -> 'a) -> 'a
  (* execute given function with the environment variable TZ set to the
     specified value.

     Not thread-safe.
  *)

val utc_of_day   : t -> Util_dateonly.t  -> Util_time.t
val utc_of_local : t -> Util_localtime.t -> Util_time.t
val local_of_utc : t -> Util_time.t -> Util_localtime.t
  (* Conversions between universal time (UTC) and local time.

     Not thread-safe.
  *)

val timezones : t list
  (* List of all timezones in a US-friendly order.
     May contain duplicates.
  *)

val timezone_mapping : (t * string) list
  (* Nicknames for common US time zones *)

val display_name : t -> string
  (* Try to use a time zone's nickname, otherwise keep it as is. *)

val tests : (string * (unit -> bool)) list
