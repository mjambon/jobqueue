exception Traced of exn * string
  (** Exception stored with the stack trace corresponding to where it
      was caught. *)

val string_of_exn : ?earlier_trace:string -> exn -> string
  (** Nice backtrace without repeats
      + string representation of the exception.
      [Traced] exceptions are unwrapped so as to form a continuous stack trace.
      [earlier_trace] if given, is prepended to the current stack trace.
  *)

val trace : ?earlier_trace:string -> exn -> string
  (** Concatenation of the exception constructor **without** arguments
      and the stack trace. The purpose of this is to group identical errors
      meaningfully.
      [earlier_trace] if given, is prepended to the current stack trace.
  *)

val make_traced : exn -> exn
  (** Wrap the given exception within a [Traced] exception
      which stores the current stack trace. *)

val trace_hash : exn -> string
  (** 32-bit, hex-encoded hash of [trace e] (see above),
      whose purpose is to identify the error. *)
