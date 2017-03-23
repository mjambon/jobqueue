(*
  Conversion from/to RFC-3339 compatible timestamp.
  If present, any timezone suffix is ignored.
*)

type t = private {
  timezone: Util_timezone.t;
  year: int;
  month: int;
  day: int;
  hour: int;
  min: int;
  sec: float;

  string: string;
    (*
       Formatted as an RFC-3339 date-time without timezone offset e.g.,

         2017-03-21T14:08:23.480
    *)

  wday: int;
    (* Day of the week, where Sunday is 0, Monday is 1, etc. *)

  yday: int;
    (* Day of the year, within [0, 365] *)

  absolute_time: float;
    (* Seconds since January 1, 1970 *)

  absolute_day: int;
    (* Day 0 is January 1, 1970, day 1 is January 2, 1970. etc.
       Can be used to determine if a calendar day is tomorrow, today, etc. *)
}

val create :
  timezone:Util_timezone.t ->
  year:int -> month:int -> day:int ->
  hour:int -> min:int -> sec:float -> t

val set_time :
  ?hour:int -> ?min:int -> ?sec:float ->
  t -> t

val of_string : timezone:Util_timezone.t -> string -> t
val of_string_opt : timezone:Util_timezone.t -> string -> t option
val to_string : t -> string

val of_pair :
  timezone:Util_timezone.t ->
  Util_dateonly.t -> Util_timeonly.t -> t

val to_pair : t -> Util_dateonly.t * Util_timeonly.t

val dateonly : t -> Util_dateonly.t
val timeonly : t -> Util_timeonly.t

val of_float : timezone:Util_timezone.t -> float -> t
val of_utc : timezone:Util_timezone.t -> Util_time.t -> t
val to_float : t -> float
val to_utc : t -> Util_time.t
  (*
     Seconds since 1970, UTC.
  *)

val to_utc : t -> Util_time.t
val of_utc : timezone:Util_timezone.t -> Util_time.t -> t
val of_day : timezone:Util_timezone.t -> Util_dateonly.t  -> t
  (* Conversions between universal time (UTC) and local time.

     Not thread-safe.
  *)

val format : fmt:string -> t -> string
  (* Format a date using Netdate.format. See documentation at URL below.
     http://projects.camlcity.org\
       /projects/dl/ocamlnet-3.2/doc/html-main/Netdate.html
  *)

(*
   Same functionality as above, excluding the functions that
   don't make sense without a valid timezone.
*)
module No_timezone : sig
  type localtime = t
  type t = localtime

  (* Functions used by atdgen *)
  val wrap : string -> t
  val unwrap : t -> string

  val of_string : string -> t
  val of_string_opt : string -> t option
  val to_string : t -> string

  val create :
    year:int -> month:int -> day:int ->
    hour:int -> min:int -> sec:float -> t

  val set_time :
    ?hour:int -> ?min:int -> ?sec:float ->
    t -> t

  val of_pair : Util_dateonly.t -> Util_timeonly.t -> t
  val to_pair : t -> Util_dateonly.t * Util_timeonly.t

  val dateonly : t -> Util_dateonly.t
  val timeonly : t -> Util_timeonly.t

  val format : fmt:string -> t -> string
end

val tests : (string * (unit -> bool)) list
