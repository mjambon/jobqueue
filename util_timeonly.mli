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

val tests : (string * (unit -> bool)) list
