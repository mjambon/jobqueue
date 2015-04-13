(*
   NOTES ON EXPIRATION TIME (copied from memcache-ocaml doc)

   Some commands involve a client sending some kind of expiration time
   (relative to an item or to an operation requested by the client) to the
   server. In all such cases, the actual value sent may either be Unix time
   (number of seconds since January 1, 1970, as a 32-bit value), or a number
   of seconds starting from current time. In the latter case, this number of
   seconds may not exceed 60*60*24*30 (number of seconds in 30 days); if the
   number sent by a client is larger than that, the server will consider it to
   be real Unix time value rather than an offset from current time.

   exptime is expiration time. If it's 0, the item never expires (although it
   may be deleted from the cache to make place for other items). If it's
   non-zero (either Unix time or offset in seconds from current time), it is
   guaranteed that clients will not be able to retrieve this item after the
   expiration time arrives (measured by server time). The default value is 0
   (never).
*)


type key = string
type value = string

(*
   Add the mapping (key, value) to memcached, replacing any existing mapping.
   If exptime is specified, the mapping will disappear at that time.
*)
val store : key -> ?exptime:int -> value -> unit Lwt.t

(*
   For fetching, when do we update the cache?
*)
type refresh =
  | Missing
      (* Default choice: update the cached value only if the key is
         currently missing from the cache. *)
  | Before
      (* Ignore the current cached value, calling the function to obtain
         it instead. Store the result in the cache and return it. Used to
         bypass the cache when refreshing is important. *)
  | After
      (* Return the current cached value, but perform an update of the
         cache in the background, without blocking. Used to obtain a quick
         result while still refreshing the cache for next time. *)
(*
   Get a value from the cache if possible, otherwise call the given function.
   The refresh strategy dictates when to update the cache.
   If a new entry is added, it will have the supplied expiration time.
*)
val cached_fetch :
  ?refresh:refresh -> ?exptime:int -> key ->
  ('a -> value Lwt.t) -> 'a -> value Lwt.t

