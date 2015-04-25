val string_of_exn : exn -> string
  (** Nice backtrace without repeats
      + string representation of the exception *)

val trace : exn -> string
  (** Concatenation of the exception constructor without arguments
      and the stack trace. The purpose of this is to group identical errors
      meaningfully.
  *)

val trace_hash : exn -> string
  (** 32-bit, hex-encoded hash of [trace e] (see above),
      whose purpose is to identify the error. *)
