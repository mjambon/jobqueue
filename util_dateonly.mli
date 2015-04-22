(*
  Conversion from/to RFC-3339 compatible date of the form YYYY-MM-DD.
*)
type t = private {
  year : int;
  month : int;
  day : int;
  string : string;
}

val create : year:int -> month:int -> day:int -> t

val compare : t -> t -> int
val now : unit -> t
val is_past : t -> bool
val is_future : t -> bool

val of_float : float -> t
val to_float : t -> float

val previous_day : t -> t

val of_string : string -> t
val of_string_opt : string -> t option
val to_string : t -> string

val wrap : string -> t
val unwrap : t -> string

val tests : (string * (unit -> bool)) list
