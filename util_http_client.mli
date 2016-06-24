val trace : bool ref
  (** Log all requests and responses (default: false) *)

type response =
  (Cohttp.Code.status_code * (string * string) list * string)

val wrap :
  ?headers:(string * string) list ->
  ?body:string ->
  Cohttp.Code.meth ->
  Uri.t -> response Lwt.t

val get :
  ?headers:(string * string) list ->
  Uri.t -> response Lwt.t
val post :
  ?headers:(string * string) list ->
  ?body:string ->
  Uri.t -> response Lwt.t
val head :
  ?headers:(string * string) list ->
  Uri.t -> response Lwt.t
val delete :
  ?headers:(string * string) list ->
  ?body:string ->
  Uri.t -> response Lwt.t
val put :
  ?headers:(string * string) list ->
  ?body:string ->
  Uri.t -> response Lwt.t
val patch :
  ?headers:(string * string) list ->
  ?body:string ->
  Uri.t -> response Lwt.t

val post_form: Uri.t -> (string * string) list -> response Lwt.t
val post_form': Uri.t -> (string * string list) list -> response Lwt.t
(* (string * string) list is more convenient, while
   (string * string list) list is used in `Uri`. *)

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
    ?body:string ->
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

module Original : Wrapped with type result = response

module type Wrapper = sig
  type orig_result
  type result
  val wrap : (unit -> orig_result Lwt.t) -> result Lwt.t
end

module Wrap
    (U: Wrapped)
    (W: Wrapper with type orig_result = U.result):
  Wrapped with type result = W.result

(** Module matching the signature expected by elasticsearch:
    the URI is a string and the response status is an int. *)
module Elasticsearch_lwt :
sig
  type uri = string
  type response = (int * (string * string) list * string)

  type 'a computation = 'a Lwt.t
  val bind : 'a computation -> ('a -> 'b computation) -> 'b computation
  val return : 'a -> 'a computation
  val head :
    ?headers:(string * string) list ->
    uri -> response option computation
  val get :
    ?headers:(string * string) list ->
    uri -> response option computation
  val post :
    ?headers:(string * string) list ->
    ?body:string ->
    uri -> response option computation
  val delete :
    ?headers:(string * string) list ->
    uri -> response option computation
  val put :
    ?headers:(string * string) list ->
    ?body:string ->
    uri -> response option computation
end
