(*
  Conversion from/to RFC-3339 compatible timestamp.
  If present, any timezone suffix is ignored.
*)

type t = private {
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
  year:int -> month:int -> day:int ->
  hour:int -> min:int -> sec:float -> t

val set_time :
  ?hour:int -> ?min:int -> ?sec:float ->
  t -> t

val of_string : string -> t
val of_string_opt : string -> t option
val to_string : t -> string

val wrap : string -> t
val unwrap : t -> string

val of_pair : Util_dateonly.t -> Util_timeonly.t -> t
val to_pair : t -> Util_dateonly.t * Util_timeonly.t

val dateonly : t -> Util_dateonly.t
val timeonly : t -> Util_timeonly.t

(* from/to unixtime (seconds since 1970, UTC) *)
val of_float : float -> t
val to_float : t -> float

val of_unix_tm : Unix.tm -> float -> t
val to_unix_tm : t -> Unix.tm * float
  (* Convert from/to Unix.tm record and remaining fraction of one second. *)

val fpart : float -> float
  (*
     Return what needs to be subtracted in order to round down to the
     nearest integer, i.e. x -> (x -. floor x).

     1.3 -> 0.3
     -1.3 -> 0.7
  *)

val format : fmt:string -> t -> string
  (* Format a date using Netdate.format. See documentation at URL below.
     http://projects.camlcity.org\
       /projects/dl/ocamlnet-3.2/doc/html-main/Netdate.html
  *)

val tests : (string * (unit -> bool)) list
