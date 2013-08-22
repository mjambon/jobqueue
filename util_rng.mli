(** Random number generator *)

val random : int -> int
  (** Generate a bounded random number *)

val uuid : unit -> string
  (** Make a version 4 UUID.
      Java's java.util.UUID.randomeUUID() uses this version *)

val string : int -> string
  (** Make a random string of the given length in bytes *)

val hex : int -> string
  (** Make a random string of the given length in bytes and hex-encode it,
      doubling its length. *)
