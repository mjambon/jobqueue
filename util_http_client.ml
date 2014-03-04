open Printf

type response =
  (Cohttp.Code.status_code * (string * string) list * string)

let trace = ref false

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

let wrap ?(headers = []) ?body meth uri =
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
      | Some s -> Cohttp_lwt_body.body_of_string s
  in
  let x =
    Lwt.bind
      (Cohttp_lwt_unix.Client.call ~headers ?body ~chunked:false meth uri)
      (function
        | None ->
            let url_string = Uri.to_string uri in
            failwith ("HTTP client: no response for " ^ url_string)
        | Some (resp, body) ->
            Lwt.bind
              (Cohttp_lwt_body.string_of_body body)
              (fun body_string ->
                let status = Cohttp_lwt_unix.Response.status resp in
                let resp_headers = Cohttp_lwt_unix.Response.headers resp in
                let t2 = Unix.gettimeofday () in
                if !trace then
                  print_resp (Lazy.force req_id)
                    status resp_headers body_string (t2 -. t1);
                Lwt.return (
                  status,
                  Cohttp.Header.to_list resp_headers,
                  body_string
                )
              )
      )
  in
  Lwt.catch
    (fun () -> x)
    (fun e ->
      let backtrace = Util_exn.string_of_exn e in
      let url_string = Uri.to_string uri in
      Printf.eprintf "[error] Exception in Cohttp client with URI %s: %s\n%!"
        url_string backtrace;
      failwith ("HTTP client error on " ^ url_string)
    )

let head ?headers uri = wrap ?headers `HEAD uri
let get ?headers uri = wrap ?headers `GET uri
let post ?headers ?body uri = wrap ?headers ?body `POST uri
let head ?headers uri = wrap ?headers `HEAD uri
let delete ?headers uri = wrap ?headers `DELETE uri
let put ?headers ?body uri = wrap ?headers `PUT ?body uri
let patch ?headers ?body uri = wrap ?headers `PATCH ?body uri
