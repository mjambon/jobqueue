val trace : bool ref
  (** Log all requests and responses (default: false) *)

type response =
  (Cohttp.Code.status_code * (string * string) list * string)

val head :
  ?headers:(string * string) list ->
  Uri.t -> response Lwt.t
val get :
  ?headers:(string * string) list ->
  Uri.t -> response Lwt.t
val post :
  ?headers:(string * string) list ->
  ?body:string ->
  ?chunked:bool ->
  Uri.t -> response Lwt.t
val head :
  ?headers:(string * string) list ->
  Uri.t -> response Lwt.t
val delete :
  ?headers:(string * string) list ->
  Uri.t -> response Lwt.t
val put :
  ?headers:(string * string) list ->
  ?body:string ->
  ?chunked:bool ->
  Uri.t -> response Lwt.t
