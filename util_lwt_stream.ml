open Lwt

let rec iter_stream chunk_size stream f =
  Lwt_stream.nget chunk_size stream >>= function
  | [] -> return ()
  | l ->
      Util_conc.iter l f >>= fun () ->
      iter_stream chunk_size stream f

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

let find_minimum cmp get_value get_key l =
  match l with
  | [] ->
      invalid_arg "Util_lwt_stream.find_optimum"
  | first_lazy_v :: more ->
      get_value first_lazy_v >>= fun first_v ->
      let first_k = get_key first_v in
      Lwt_list.fold_left_s (fun ((min_key, min_v, _) as acc) lazy_v ->
        get_value lazy_v >>= fun v ->
        let k = get_key v in
        if cmp k min_key < 0 then
          return (k, v, lazy_v)
        else
          return acc
      ) (first_k, first_v, first_lazy_v) more
      >>= fun (opt_k, opt_v, stream) ->
      match opt_v with
      | None -> return None
      | Some v -> return (Some (v, stream))

let merge ?(cmp = compare) ~get_key streams =
  let get_opt_value stream =
    Lwt_stream.peek stream
  in
  let get_opt_key opt_elt =
    match opt_elt with
    | None -> None
    | Some x -> Some (get_key x)
  in
  (* order by Some first *)
  let cmp_opt a b =
    match a, b with
    | Some a, Some b -> cmp a b
    | None, None -> 0
    | Some _, None -> -1
    | None, Some _ -> 1
  in
  match streams with
  | [] ->
      Lwt_stream.of_list []
  | _ ->
      let rec next () =
        find_minimum cmp_opt get_opt_value get_opt_key streams
        >>= function
        | None ->
            return None
        | Some (v, stream) ->
            Lwt_stream.junk stream >>= fun () ->
            return (Some v)
      in
      Lwt_stream.from next

(* for testing *)
let merge_lists lists =
  let get_key x = x in
  let streams = List.map Lwt_stream.of_list lists in
  let st = merge ~get_key streams in
  Lwt_main.run (Lwt_stream.to_list st)

let test_merge () =
  assert (merge_lists [] = []);
  assert (merge_lists [[]] = []);
  assert (merge_lists [[1]; [2]] = [1;2]);
  assert (merge_lists [[2]; [1]] = [1;2]);
  assert (merge_lists [[2]; [1;1]] = [1;1;2]);

  assert (merge_lists [
    [1;2;3;4];
    [2;3;5];
    []
  ] = [1;2;2;3;3;4;5]);

  (* out-of-order input is tolerated, giving funny results *)
  assert (merge_lists [[1;2]; [3;0]] = [1;2;3;0]);
  assert (merge_lists [[1;3]; [2;0]] = [1;2;0;3]);

  true

let tests = [
  "paged stream", test_paged_stream;
  "merge", test_merge;
]
