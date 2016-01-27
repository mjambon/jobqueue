(*
   Linux-specific features, typically used as a last resort
   in production only.

   All these functions are safe to run on MacOS, in which case
   they will return None or some value that indicates that a result
   is not available. Exceptions are reserved for errors.
*)

val get_resident_memory_size : int -> float option
  (*
     Get the resident memory size of the given process.
     Return None if the /procPID/status file doesn't exists,
     raise an exception if the file exists but can't be parsed.
  *)

val tests : (string * (unit -> bool)) list
