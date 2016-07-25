(*
  Conversion from/to RFC-3339 compatible time of the day of the form HH:MM:SS.
*)
type t = private {
  hour : int;
  min : int;
  sec : float;
  string : string;
}

val create : hour:int -> min:int -> sec:float -> t

val of_string : string -> t
val of_string_opt : string -> t option
val to_string : t -> string

val wrap : string -> t
val unwrap : t -> string

val of_float : float -> t
  (* Interpret the input as a number of seconds since midnight on some day.
     The result is guaranteed to be in the range 0:00:00.000-23:59:59.999
  *)

val to_float : t -> float
  (* Return the number of seconds since the beginning of the day. *)

val format : fmt:string -> t -> string
  (* Format a time using Netdate.format. See documentation at URL below.
     http://projects.camlcity.org\
       /projects/dl/ocamlnet-3.2/doc/html-main/Netdate.html

     Don't use this to format the date part, or you'll get 1970-01-01.
  *)

val tests : (string * (unit -> bool)) list
