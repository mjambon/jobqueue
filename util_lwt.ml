open Lwt

let do_if bool f =
  if bool then f ()
  else return ()

let bind2 a b func =
  a >>= fun a' ->
  b >>= fun b' ->
  func a' b'

let bind3 a b c func =
  a >>= fun a' ->
  b >>= fun b' ->
  c >>= fun c' ->
  func a' b' c'

let bind4 a b c d func =
  a >>= fun a' ->
  b >>= fun b' ->
  c >>= fun c' ->
  d >>= fun d' ->
  func a' b' c' d'

let bind5 a b c d e func =
  a >>= fun a' ->
  b >>= fun b' ->
  c >>= fun c' ->
  d >>= fun d' ->
  e >>= fun e' ->
  func a' b' c' d' e'

let bind6 a b c d e f func =
  a >>= fun a' ->
  b >>= fun b' ->
  c >>= fun c' ->
  d >>= fun d' ->
  e >>= fun e' ->
  f >>= fun f' ->
  func a' b' c' d' e' f'

let join2 a b =
  bind2 a b (fun a b -> return (a, b))

let join3 a b c =
  bind3 a b c (fun a b c -> return (a, b, c))

let join4 a b c d =
  bind4 a b c d (fun a b c d -> return (a, b, c, d))

let join5 a b c d e =
  bind5 a b c d e (fun a b c d e -> return (a, b, c, d, e))

let or_ l =
  Lwt_list.exists_p (fun x -> x) l

let and_ l =
  Lwt_list.for_all_p (fun x -> x) l

let rec find_map_left l f =
  match l with
  | [] -> return None
  | x :: tl ->
      f x >>= function
      | None -> find_map_left tl f
      | Some y as result -> return result

let find_map_right l f =
  find_map_left (List.rev l) f

let map_default default opt f =
  match opt with
  | None -> return default
  | Some x -> f x

let rec repeat n op =
  if 0 < n then op n >>= fun () -> repeat (n-1) op
  else return ()

let rec iter_stream chunk_size stream f =
  Lwt_stream.nget chunk_size stream >>= function
  | [] -> return ()
  | l ->
      Util_conc.iter l f >>= fun () ->
      iter_stream chunk_size stream f

let gethostbyname hostname =
  catch
    (fun () ->
       Lwt_unix.gethostbyname hostname
    )
    (fun e ->
       match Trax.unwrap e with
       | Not_found ->
           failwith ("Cannot resolve host " ^ hostname)
       | e ->
           Trax.raise __LOC__ e
    )

let create_paged_stream acc page_f =
  let buf = ref [] in
  let acc = ref acc in
  let want_refill = ref true in
  let rec get_item () =
    match !buf with
    | x::xs ->
        buf := xs;
        return (Some x)
    | [] ->
        if not !want_refill then
          return None
        else
          page_f !acc >>= fun (next_acc, items, continue) ->
          want_refill := continue;
          acc := next_acc;
          buf := items;
          get_item ()
  in
  Lwt_stream.from get_item

let test_paged_stream () =
  let create_service pages =
    assert (pages <> []);
    let page_array = Array.of_list pages in
    let get_page page_number =
      let page = page_array.(page_number) in
      let next_page_number =
        let next = page_number + 1 in
        if next >= Array.length page_array then None
        else Some next
      in
      return (page, next_page_number)
    in
    get_page
  in
  let get_pages_back pages =
    let get_page = create_service pages in
    let stream =
      create_paged_stream 0 (fun page_number ->
        get_page page_number >>= fun (items, next_page_number) ->
        match next_page_number with
        | None -> return (-1, items, false)
        | Some n -> return (n, items, true)
      )
    in
    Lwt_stream.to_list stream
  in
  let check pages =
    Lwt_main.run (
      get_pages_back pages >>= fun result ->
      return (result = List.flatten pages)
    )
  in
  assert (check [[]]);
  assert (check [[]; [1]]);
  assert (check [[1;2]; [3]; [4]]);
  assert (check [[1;2]; []; []; [4;5;6]]);
  true

let with_retries delays f =
  let rec loop i delays =
    f i >>= function
    | None ->
        (match delays with
         | delay :: delays ->
             Lwt_unix.sleep delay >>= fun () ->
             loop (i+1) delays
         | [] ->
             return None
        )
    | Some result -> return (Some (i, result))
  in
  loop 0 delays

let tests = [
  "paged stream", test_paged_stream;
]
