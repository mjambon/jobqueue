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
        | Some comp -> comp (* cached computation in progress *)
        | None ->
            (* no computation in progress *)
            let comp = f () in
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
    let t1 =
      catch
        (fun () -> z () >>= fun _ -> assert false)
        (function
          | Int n -> assert (n = 1); return ()
          | e -> raise e)
    in
    let t2 =
      catch
        (fun () -> z () >>= fun _ -> assert false)
        (function
          | Int n -> assert (n = 1); return ()
          | e -> raise e)
    in
    Lwt.join [ t1; t2 ] >>= fun () ->

    Lwt_list.map_p z [ (); (); () ] >>= fun result_from_attempt2 ->
    return (result_from_attempt2 = [ 2; 2; 2 ])
  )
