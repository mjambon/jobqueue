open Printf
open Lwt

(* Use this in the body of deep recursive functions
   to prevent stack overflows and memory leaks. *)
let ( >>=! ) = Lwt.bind

let make_counter () =
  let counter = ref 0 in
  fun () ->
    if !counter = -1 then
      failwith "int overflow";
    incr counter;
    !counter

(*
   Compute the stream elements sequentially,
   apply a function to each element concurrently.

   If too many jobs (> max_threads) are already busy processing elements,
   we wait until one job finishes. This is signaled via finished_job.

   If one job fails with an exception, all the other jobs are killed.

   The state of the main thread is set either once all the jobs
   are complete or when a job fails with an exception.
*)
let iter max_threads stream f =
  if max_threads <= 0 then
    invalid_arg "Util_lwt_stream.iter: max_threads must be positive";
  let finished_job = Lwt_condition.create () in
  let running = ref 0 in
  (* Jobs that are running concurrently or just finished
     and were not removed from this table yet.
     This is used for killing concurrent jobs when one job terminates
     with an exception. *)
  let jobs = Hashtbl.create max_threads in
  let get_id = make_counter () in
  let add_job x =
    let id = get_id () in
    Hashtbl.add jobs id x;
    (* Eventual auto-removal from the table *)
    Lwt.async (fun () ->
      Lwt.finalize
        (fun () -> x)
        (fun () -> Hashtbl.remove jobs id; return ())
    )
  in
  let waiter, awakener = Lwt.wait () in
  let is_asleep () =
    Lwt.state waiter = Sleep
  in
  let stream_length = ref 0 in
  let started_all_jobs = ref false in
  let completed = ref 0 in
  let maybe_finish () =
    if !started_all_jobs && !completed = !stream_length then
      if is_asleep () then
        Lwt.wakeup awakener ()
  in
  let mark_job_running () =
    if !running < max_threads then (
      (* One of the first conc jobs to start *)
      incr running;
      return ()
    )
    else (
      (* Wait until another job finishes *)
      Lwt_condition.wait finished_job >>=! fun () ->
      assert (!running < max_threads);
      incr running;
      return ()
    )
  in
  let mark_job_complete () =
    decr running;
    incr completed;
    (* Awaken the main thread (waiter) if all the other jobs
       were started and completed. *)
    maybe_finish ();
    Lwt_condition.signal finished_job ()
  in
  let mark_job_failed e =
    (* Set the state of the main thread (waiter) to this exception *)
    if is_asleep () then
      Lwt.wakeup_exn awakener e;
    (* Kill all the running jobs *)
    Hashtbl.iter (fun id x -> Lwt.cancel x) jobs
  in
  let launch_job x =
    catch
      (fun () ->
         mark_job_running () >>=! fun () ->
         f x >>=! fun () ->
         mark_job_complete ();
         return ()
      )
      (fun e ->
         mark_job_failed e;
         return ()
      )
  in
  Lwt_stream.iter (fun x ->
    incr stream_length;
    let background_job = launch_job x in
    Lwt.async (fun () -> background_job);
    add_job background_job
  ) stream
  >>=! fun () ->
  started_all_jobs := true;
  (* If the stream was empty or if all the jobs already completed,
     we need to awaken the main thread (waiter) now. *)
  maybe_finish ();
  waiter

exception Test_exn of string

let test_iter () =
  let stream_of_list l =
    let q = ref l in
    Lwt_stream.from (fun () ->
      match !q with
      | [] -> return None
      | x :: l ->
          (* Make sure stream elements are evaluated asynchronously *)
          Lwt_unix.sleep 1e-6 >>=! fun () ->
          q := l;
          return (Some x)
    )
  in
  let async_thread (i, dt) =
    printf "start %i\n%!" i;
    Lwt_unix.sleep dt >>=! fun () ->
    printf "end %i\n%!" i;
    return ()
  in
  let sync_thread i =
    printf "start-end %i\n%!" i;
    return ()
  in
  let async_thread_exn (i, dt) =
    printf "start %i\n%!" i;
    Lwt_unix.sleep dt >>=! fun () ->
    raise (Test_exn (sprintf "exception in thread %i" i))
  in
  let sync_thread_exn i =
    printf "start %i\n%!" i;
    raise (Test_exn (sprintf "exception in thread %i" i))
  in

  let foreach l f = List.iter f l in

  let run_iter max_threads stream f =
    Lwt_main.run (
      let iteration = iter max_threads stream f in
      let timer =
        Lwt_unix.sleep 10. >>=! fun () ->
        assert false
      in
      pick [iteration; timer]
    )
  in
  let run_iter_success max_threads list f =
    foreach [ "synchronous stream", Lwt_stream.of_list list;
              "asynchronous stream", stream_of_list list ]
      (fun (text, stream) ->
         print_endline text;
         run_iter max_threads stream f
      )
  in
  let run_iter_exn max_threads list f =
    foreach [ "synchronous stream", Lwt_stream.of_list list;
              "asynchronous stream", stream_of_list list ]
      (fun (text, stream) ->
         print_endline text;
         try run_iter max_threads stream f
         with Test_exn _ -> ()
      )
  in

  run_iter_success 2 [] async_thread;
  run_iter_success 2 [1, 0.01; 2, 0.001; 3, 0.001; 4, 0.001] async_thread;
  run_iter_success 2 [1;2;3;4;5;6] sync_thread;
  run_iter_success 5 [1, 0.001; 2, 0.001] async_thread;
  run_iter_success 5 [1;2] sync_thread;
  run_iter_exn 3 [1, 0.003; 2, 0.001; 3, 0.001; 4, 0.001] async_thread_exn;
  run_iter_exn 3 [1;2;3;4;5] sync_thread_exn;
  true

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
          page_f !acc >>=! fun (next_acc, items, continue) ->
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

let merge ?(cmp = compare) ?(exn_handler = fail) ~get_key streams =
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
        catch
          (fun () ->
             find_minimum cmp_opt get_opt_value get_opt_key streams
             >>=! function
             | None ->
                 return None
             | Some (v, stream) ->
                 Lwt_stream.junk stream >>= fun () ->
                 return (Some v)
          )
          exn_handler
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
  "iter", test_iter;
  "paged stream", test_paged_stream;
  "merge", test_merge;
]
