(*
   Timezones and conversion between universal time and local times.
*)

type tz = string
  (* timezone name, e.g. "America/Los_Angeles" *)

val is_supported : tz -> bool
  (* whether the timezone is in our list *)

val timezone_of_string : string -> tz
  (* check that the timezone is supported and return the same string
     or fail with an exception. *)

val fallback_timezone : tz

val with_timezone : tz -> (unit -> 'a) -> 'a
  (* execute given function with the environment variable TZ set to the
     specified value.

     Not thread-safe.
  *)

val utc_of_day   : tz -> Util_dateonly.t  -> Util_time.t
val utc_of_local : tz -> Util_localtime.t -> Util_time.t
val local_of_utc : tz -> Util_time.t -> Util_localtime.t
  (* Conversions between universal time (UTC) and local time.

     Not thread-safe.
  *)

val timezones : tz list
  (* List of all timezones in a US-friendly order.
     May contain duplicates.
  *)

val timezone_mapping : (tz * string) list
  (* Nicknames for common US time zones *)

val display_name : tz -> string
  (* Try to use a time zone's nickname, otherwise (Time zone: tz) *)

val day_of_week : tz -> Util_time.t -> int
  (* 0 = Sunday, 1 = Monday, ..., 6 = Saturday, 7 = Sunday *)

val tests : (string * (unit -> bool)) list
