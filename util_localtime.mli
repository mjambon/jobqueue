type t = private {
  year : int;
  month : int;
  day : int;
  hour : int;
  min : int;
  sec : float;
  string : string;
}

val create :
  year:int -> month:int -> day:int ->
  hour:int -> min:int -> sec:float -> t

(*
  Conversion from/to RFC-3339 compatible timestamp.
  If present, any timezone suffix is ignored.
*)
val of_string : string -> t
val of_string_opt : string -> t option
val to_string : t -> string

val wrap : string -> t
val unwrap : t -> string

val tests : (string * (unit -> bool)) list
