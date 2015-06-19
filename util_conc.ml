open Lwt

let default_conc = 20

(*
   Algorithm outline:
   - We start 20 jobs
   - Each time a job is done we signal another job that it can start.
*)
let map ?(conc = default_conc) l f =
  if conc <= 0 then
    invalid_arg "Util_conc.map";
  let finished_job = Lwt_condition.create () in
  let running = ref 0 in
  let waiting = ref 0 in
  Lwt_list.map_p (fun x ->
    Lwt.finalize
      (fun () ->
         if !running < conc then (
           (* One of the first conc jobs to start *)
           incr running;
           f x
         )
         else (
           (* Wait until another job finishes *)
           incr waiting;
           Lwt_condition.wait finished_job >>= fun () ->
           decr waiting;
           f x
         )
      )
      (fun () ->
         if !waiting > 0 then
           (* Signal another job that we're done with this one *)
           Lwt_condition.signal finished_job ()
         else
           (* We can't signal the next job because it doesn't exist yet
              (or they're all running, which isn't a problem).
              We allow one job to be started without being signalled. *)
           decr running;

         return ()
      )
  ) l

let iter ?conc l f =
  map ?conc l f >>= fun l ->
  return ()

let filter_map ?conc l f =
  map ?conc l f >>= fun l' ->
  return (BatList.filter_map (fun o -> o) l')

let filter ?conc l f =
  map ?conc l (fun x ->
    f x >>= fun b ->
    return (
      if b then Some x
      else None
    )
  ) >>= fun l' ->
  return (BatList.filter_map (fun o -> o) l')

let exists ?conc l f =
  map ?conc l f >>= fun l ->
  return (List.mem true l)

let for_all ?conc l f =
  map ?conc l f >>= fun l ->
  return (not (List.mem false l))


(** Tests **)

(*
   Each test is done in two variants:
   - jobs that return immediately without going through the lwt scheduler
   - jobs that go through the lwt scheduler, which is more realistic
*)

type test_param = {
  test_sleep : unit -> unit Lwt.t;
  test_return : 'a. 'a -> 'a Lwt.t;
}

let sync_param = {
  test_sleep = return;
  test_return = return;
}

let async_param = {
  test_sleep = (fun () -> Lwt_unix.sleep 0.);
  test_return = (fun x -> Lwt_unix.sleep 0. >>= fun () -> return x);
}

let test_map p () =
  let conc = 2 in
  let n = ref 0 in
  let t =
    map ~conc [1;2;3;4;5;6;7] (fun i ->
      incr n;
      assert (!n <= conc);
      p.test_sleep () >>= fun () ->
      decr n;
      p.test_return (i * 2)
    )
  in
  Util_lwt_main.run t = [2;4;6;8;10;12;14]

let test_filter_map p () =
  let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
  let t =
    filter_map ~conc:5 l (fun x ->
      if x mod 2 = 0 then p.test_return (Some (-x))
      else p.test_return None
    ) in
  Util_lwt_main.run t = [ 0; -2; -4; -6 ]

let test_filter p () =
  let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
  let t = filter ~conc:5 l (fun x -> p.test_return (x mod 2 = 0)) in
  Util_lwt_main.run t = [ 0; 2; 4; 6 ]

let test_exists p () =
  let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
  let t = exists ~conc:5 l (fun x -> p.test_return (x = 6)) in
  Util_lwt_main.run t = true

let test_not_exists p () =
  let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
  let t = exists ~conc:5 l (fun x -> p.test_return (x = 8)) in
  Util_lwt_main.run t = false

let test_for_all p () =
  let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
  let t = for_all ~conc:5 l (fun x -> p.test_return (x < 8)) in
  Util_lwt_main.run t = true

let test_not_for_all p () =
  let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
  let t = for_all ~conc:5 l (fun x -> p.test_return (x < 6)) in
  Util_lwt_main.run t = false

let tests = [
  "map sync", test_map sync_param;
  "map async", test_map async_param;
  "filter sync", test_filter sync_param;
  "filter async", test_filter async_param;
  "filter_map sync", test_filter_map sync_param;
  "filter_map async", test_filter_map async_param;
  "exists sync", test_exists sync_param;
  "exists async", test_exists async_param;
  "not exists sync", test_not_exists sync_param;
  "not exists async", test_not_exists async_param;
  "for_all sync", test_for_all sync_param;
  "for_all async", test_for_all async_param;
  "not for_all sync", test_not_for_all sync_param;
  "not for_all async", test_not_for_all async_param;
]
