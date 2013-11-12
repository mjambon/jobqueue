(*
  Conversion from/to RFC-3339 compatible timestamp.
  If present, any timezone suffix is ignored.
*)

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

val tests : (string * (unit -> bool)) list
