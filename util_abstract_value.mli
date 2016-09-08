(*
   An OCaml value whose sole purpose is to raise the exception
   `Invalid_argument "equal: functional value"` when a comparison
   is attempted using the polymorphic comparison functions and operators:
   Pervasives.compare, (=), (<), etc.

   It is meant to be used as the first field of a record for which
   we want to block the use of polymorphic comparison functions.
*)

type t

val create : unit -> t

val tests : (string * (unit -> bool)) list
