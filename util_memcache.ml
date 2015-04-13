open Log
open Lwt
open Printf

type key = string
type value = string
type refresh = Missing | Before | After

let server = "127.0.0.1"
let port = 11211
let conn = ref None

let connect ?(server = server) ?(port = port) () =
  match !conn with
  | None ->
      Memcache.open_connection server port >>= fun connection ->
      conn := Some connection;
      return ()
  | Some connection ->
      logf `Warning "Util_memcache.connect: already connected";
      return ()

let disconnect =
  match !conn with
  | None -> return ()
  | Some connection -> Memcache.close_connection connection

let with_connection f =
  match !conn with
  | None ->
      failwith "Util_memcache.cached_fetch: connection is closed";
  | Some connection -> f connection

let get_cached_value connection key =
  Memcache.get connection key >>= function
  | None -> return None
  | Some (k, (_flags, value)) ->
      assert (k = key);
      return (Some value)

let try_store connection key ?exptime value =
  let open Memcache in
  set connection key ?exptime value >>= function
  | STORED -> return () (* Expected case *)
  | NOT_STORED ->
      let err =
        sprintf "Util_memcache.try_store: couldn't set (%s, %s)"
          key value
      in
      failwith err
  | EXISTS -> assert false (* Only applies for cas command *)
  | NOT_FOUND -> assert false (* Only applies for cas command *)

let store key ?exptime value = 
  with_connection (fun conn ->
    try_store conn key ?exptime value
  )

let try_cached_fetch ?(refresh = Missing) ?exptime connection key f args =

  (* Call f to get an updated value, insert into cache and return *)
  let update_cache () =
    f args >>= fun value ->
    try_store connection key ?exptime value >>= fun () ->
    return value
  in

  get_cached_value connection key >>= function
  | None -> update_cache ()
  | Some value ->
      match refresh with
      | Missing -> return value
      | Before -> update_cache ()
      | After -> async update_cache; return value

let cached_fetch ?refresh ?exptime key f args =
  with_connection (fun conn ->
    try_cached_fetch ?refresh ?exptime conn key f args
  )
