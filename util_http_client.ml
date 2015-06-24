open Printf
open Lwt

type response =
  (Cohttp.Code.status_code * (string * string) list * string)

let trace = ref false

(* Don't run more than this many http requests at the same time *)
let throttle = Pool.create_throttler 100

let print_headers headers =
  let l = Cohttp.Header.to_list headers in
  List.iter (fun (k, v) -> printf "  %s: %s\n" k v) l

let print_req req_id meth uri_s headers body =
  printf "[%s]\n" (Util_time.now ()).Util_time.string;
  printf "Request %s\n" req_id;
  printf "Request %s %s\n" (Cohttp.Code.string_of_method meth) uri_s;
  printf "Request headers:\n";
  print_headers headers;
  (match body with
      None -> ()
    | Some s -> printf "Request body:\n%s\n" (Util_text.prettify s);
  );
  flush stdout

let print_resp req_id status headers body latency =
  printf "[%s]\n" (Util_time.now ()).Util_time.string;
  printf "Response %s\n" req_id;
  printf "Response status: %s (%.3f s)\n"
    (Cohttp.Code.string_of_status status) latency;
  printf "Response headers:\n";
  print_headers headers;
  printf "Response body:\n%s\n" (Util_text.prettify body);
  flush stdout

let wrap_call ?(headers = []) ?body meth uri =
  let headers = Cohttp.Header.of_list headers in
  let headers =
    match Cohttp.Header.get headers "content-length", body with
        None, Some s ->
          Cohttp.Header.add headers
            "content-length"
            (string_of_int (String.length s))
      | _ -> headers
  in
  let req_id = lazy (
    sprintf "%s-%s"
      (match Uri.host uri with None -> "_" | Some s -> s)
      (Util_hex.encode (Util_rng.string 4))
  )
  in
  if !trace then
    print_req (Lazy.force req_id) meth (Uri.to_string uri) headers body;
  let t1 = Unix.gettimeofday () in
  let body =
    match body with
        None -> None
      | Some s -> Some (Cohttp_lwt_body.of_string s)
  in
  Cohttp_lwt_unix.Client.call ~headers ?body ~chunked:false meth uri
  >>= fun (resp, body) ->
  Cohttp_lwt_body.to_string body >>= fun body_string ->
  let status = Cohttp_lwt_unix.Response.status resp in
  let resp_headers = Cohttp_lwt_unix.Response.headers resp in
  let t2 = Unix.gettimeofday () in
  if !trace then
    print_resp (Lazy.force req_id)
      status resp_headers body_string (t2 -. t1);
  return (
    status,
    Cohttp.Header.to_list resp_headers,
    body_string
  )

let wrap ?headers ?body meth uri =
  throttle (fun () -> wrap_call ?headers ?body meth uri)

module type Wrapped = sig
  type result
  val get :
    ?headers:(string * string) list ->
    Uri.t -> result Lwt.t
  val post :
    ?headers:(string * string) list ->
    ?body:string ->
    Uri.t -> result Lwt.t
  val head :
    ?headers:(string * string) list ->
    Uri.t -> result Lwt.t
  val delete :
    ?headers:(string * string) list ->
    Uri.t -> result Lwt.t
  val put :
    ?headers:(string * string) list ->
    ?body:string ->
    Uri.t -> result Lwt.t
  val patch :
    ?headers:(string * string) list ->
    ?body:string ->
    Uri.t -> result Lwt.t
end

module Original = struct
  type result = response
  let head ?headers uri = wrap ?headers `HEAD uri
  let get ?headers uri = wrap ?headers `GET uri
  let post ?headers ?body uri = wrap ?headers ?body `POST uri
  let head ?headers uri = wrap ?headers `HEAD uri
  let delete ?headers uri = wrap ?headers `DELETE uri
  let put ?headers ?body uri = wrap ?headers `PUT ?body uri
  let patch ?headers ?body uri = wrap ?headers `PATCH ?body uri
end

include Original

module type Wrapper = sig
  type orig_result
  type result
  val wrap : (unit -> orig_result Lwt.t) -> result Lwt.t
end

module Wrap
    (U: Wrapped)
    (W: Wrapper with type orig_result = U.result):
  Wrapped with type result = W.result
=
struct

  type result = W.result

  let get ?headers uri =
    W.wrap (fun () -> U.get ?headers uri)
  let post ?headers ?body uri =
    W.wrap (fun () -> U.post ?headers ?body uri)
  let head ?headers uri =
    W.wrap (fun () -> U.head ?headers uri)
  let delete ?headers uri =
    W.wrap (fun () -> U.delete ?headers uri)
  let put ?headers ?body uri =
    W.wrap (fun () -> U.put ?headers ?body uri)
  let patch ?headers ?body uri =
    W.wrap (fun () -> U.patch ?headers ?body uri)
end

module Elasticsearch_lwt =
struct
  type uri = string
  type response = (int * (string * string) list * string)

  type 'a computation = 'a Lwt.t
  let bind = Lwt.bind
  let return = Lwt.return

  let wrap uri_s call =
    bind
      (call (Uri.of_string uri_s))
      (fun (status, headers, body) ->
        return (Some (Cohttp.Code.code_of_status status, headers, body))
      )

  let head ?headers s =
    wrap s (fun uri -> head ?headers uri)
  let get ?headers s =
    wrap s (fun uri -> get ?headers uri)
  let post ?headers ?body s =
    wrap s (fun uri -> post ?headers ?body uri)
  let put ?headers ?body s =
    wrap s (fun uri -> put ?headers ?body uri)
  let delete ?headers s =
    wrap s (fun uri -> delete ?headers uri)
end
