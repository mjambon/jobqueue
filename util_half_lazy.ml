open Lwt

(*
   Cache the result of a computation only if successful,
   i.e. only when it doesn't raise an exception.
*)
let create f =
  let result = ref None in
  fun () ->
    match !result with
    | Some x -> x
    | None ->
        let x = f () in
        result := Some x;
        x

(*
   Cache an asynchronous computation but only until it fails with an exception.
*)
let create_lwt f =
  let in_progress = ref None in
  let result = ref None in
  fun () ->
    match !result with
    | Some ret -> ret (* cached pre-computed result *)
    | None ->
        match !in_progress with
        | Some comp ->
            (* cached computation in progress *)
            comp
        | None ->
            (* no computation in progress *)
            let comp =
              catch f
                (fun e -> in_progress := None; Util_exn.reraise e)
            in
            in_progress := Some comp;
            comp >>= fun _res ->
            in_progress := None;
            result := Some comp;
            comp

let test_lwt_caching () =
  Lwt_main.run (
    let counter = ref 0 in
    let z =
      create_lwt (fun () ->
        incr counter;
        Lwt_unix.sleep 0.1 >>= fun () ->
        return !counter
      )
    in
    Lwt_list.map_p z [ (); (); () ] >>= fun result ->
    return (result = [ 1; 1; 1 ])
  )

exception Int of int

let test_lwt_exception () =
  Lwt_main.run (
    let counter = ref 0 in
    let z =
      create_lwt (fun () ->
        incr counter;
        Lwt_unix.sleep 0.1 >>= fun () ->
        raise (Int !counter)
      )
    in
    let wrap () =
      catch
        (fun () ->
           z () >>= fun _ ->
           assert false
        )
        (function
          | Int n -> return n
          | _ -> assert false)
    in
    Lwt_list.map_p (fun f -> f ()) [ wrap; wrap ] >>= fun l ->
    assert (l = [ 1; 1 ]);
    assert (!counter = 1);

    Lwt_list.map_s (fun f -> f ()) [ wrap; wrap ] >>= fun l ->
    assert (l = [ 2; 3 ]);
    assert (!counter = 3);

    return true
  )

let tests = [
  "lwt caching", test_lwt_caching;
  "lwt exception", test_lwt_exception;
]
