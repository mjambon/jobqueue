open Lwt

let default_conc = 20

(*
   - We start 20 jobs
   - Each time a job is done we signal another job that it can start.
*)
let map ?(conc = default_conc) l f =
  if conc <= 0 then
    invalid_arg "Util_conc.map";
  let finished_job = Lwt_condition.create () in
  let counter = ref 0 in
  Lwt_list.map_p (fun x ->
    Lwt.finalize
      (fun () ->
         if !counter < conc then (
           (* one of the first conc jobs to start *)
           incr counter;
           f x
         )
         else
           (* wait until another job finishes *)
           Lwt_condition.wait finished_job >>= fun () ->
           f x
      )
      (fun () ->
         (* signal another job that we're done with this one *)
         Lwt_condition.signal finished_job ();
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

let test_map () =
  let conc = 2 in
  let n = ref 0 in
  let t =
    map ~conc [1;2;3;4;5;6;7] (fun i ->
      incr n;
      assert (!n <= conc);
      Lwt_unix.sleep 0.01 >>= fun () ->
      decr n;
      return (i * 2)
    )
  in
  Util_lwt_main.run t = [0;2;4;6;8;10;12;14]

let tests = [
  "map", test_map;
]
