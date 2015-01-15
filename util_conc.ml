open Lwt

let default_conc = 20

let rec launch_worker running instrm f push =
  Lwt_stream.get instrm >>= function
    | None ->
        decr running;
        if !running = 0 then
          push None;
        return ()
    | Some x ->
        Lwt.catch
          (fun () ->
            f x >>= fun y ->
            return (Some y)
          )
          (fun e ->
            (try !Lwt.async_exception_hook e
             with _ -> ());
            return None
          )
        >>= fun result ->
        (match result with
          | None ->
              Lwt.ignore_result (launch_worker running instrm f push)
          | Some y ->
              (* don't launch the next job until the result is consumed *)
              let lazy_result = lazy (
                Lwt.ignore_result (launch_worker running instrm f push);
                y
              ) in
              push (Some lazy_result)
        );
        return ()

let map_stream ?(conc = default_conc) instrm f =
  if conc <= 0 then
    invalid_arg ("Util_iter.map_stream: n=" ^ string_of_int conc);
  let getterstrm, push = Lwt_stream.create () in
  let running = ref conc in
  for i = 1 to conc do
    Lwt.ignore_result (launch_worker running instrm f push)
  done;
  let outstrm = Lwt_stream.map Lazy.force getterstrm in
  outstrm

let iter_stream ?conc instrm f =
  let outstrm =
    map_stream ?conc instrm
      (fun x -> f x >>= fun b -> return (b, ()))
  in
  Lwt_stream.fold (fun (b, ()) acc -> b && acc) outstrm true

type 'a result = Val of 'a | Exn of exn

let map ?conc l f =
  let a = Array.mapi (fun i x -> (i, x)) (Array.of_list l) in
  let instrm = Lwt_stream.of_array a in
  let outstrm =
    map_stream ?conc instrm (
      fun (i, x) ->
        Lwt.catch
          (fun () -> f x >>= fun y -> return (true, (i, Val y)))
          (fun e -> return (false, (i, Exn e)))
    )
  in
  Lwt_stream.to_list outstrm >>= fun l ->
  let l =
    List.fold_left (fun acc (b, (i, result)) ->
      match result with
          Val y -> (i, y) :: acc
        | Exn e -> raise e
    ) [] l
  in
  let r = List.sort (fun (i, _) (j, _) -> compare j i) l in
  let l = List.rev_map snd r in
  return l

let iter ?conc l f =
  map ?conc l f >>= fun l' ->
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

exception Found (* private *)

(* Return true as soon as true is found *)
let exists ?conc l f =
  catch
    (fun () ->
       iter ?conc l (fun x ->
         f x >>= function
         | true -> raise Found
         | false -> return ()
       ) >>= fun () ->
       return false
    )
    (function
      | Found -> return true
      | e -> raise e
    )

(* Return false as soon as false is found *)
let for_all ?conc l f =
  catch
    (fun () ->
       iter ?conc l (fun x ->
         f x >>= function
         | false -> raise Found
         | true -> return ()
       ) >>= fun () ->
       return true
    )
    (function
      | Found -> return false
      | e -> raise e
    )

module Test =
struct
  exception Int of int
  let test_exception_order () =
    let conc = 2 in
    let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
    let t =
      Lwt.catch
        (fun () ->
          map ~conc l (fun x ->
            if x >= 3 then raise (Int x)
            else return ()
          ) >>= fun l ->
          return false
        )
        (function
          | Int (3 | 4) -> return true
          | _ -> return false
        )
    in
    Lwt_main.run t

  let test_map_order () =
    let conc = 2 in
    let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
    let t =
      map ~conc l (fun x -> return (x + 10)) >>= fun res ->
      return (res = List.map (fun x -> x + 10) l)
    in
    Lwt_main.run t

  let test_max_concurrency () =
    let debug = false in
    let conc = 3 in
    let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
    let running = ref 0 in
    let check () = !running <= conc in
    let t =
      let outstrm =
        map_stream ~conc (Lwt_stream.of_list l) (
          fun x ->
            incr running;
            if debug then
              Printf.printf "++ x=%i running=%i\n%!" x !running;
            return x
        )
      in
      let success = ref true in
      Lwt_stream.iter
        (fun x ->
          decr running;
          success := check () && !success;
          if debug then
            Printf.printf "-- x=%i running=%i\n%!" x !running;
        ) outstrm
      >>= fun () ->
      return !success
    in
    Lwt_main.run t

  let test_filter_map () =
    let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
    let t =
      filter_map ~conc:5 l (fun x ->
        if x mod 2 = 0 then return (Some (-x))
        else return None
      ) in
    Lwt_main.run t = [ 0; -2; -4; -6 ]

  let test_filter () =
    let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
    let t = filter ~conc:5 l (fun x -> return (x mod 2 = 0)) in
    Lwt_main.run t = [ 0; 2; 4; 6 ]

  let test_exists () =
    let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
    let t = exists ~conc:5 l (fun x -> return (x = 6)) in
    Lwt_main.run t = true

  let test_not_exists () =
    let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
    let t = exists ~conc:5 l (fun x -> return (x = 8)) in
    Lwt_main.run t = false

  let test_for_all () =
    let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
    let t = for_all ~conc:5 l (fun x -> return (x < 8)) in
    Lwt_main.run t = true

  let test_not_for_all () =
    let l = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
    let t = for_all ~conc:5 l (fun x -> return (x < 6)) in
    Lwt_main.run t = false

  let tests = [
    "exception order", test_exception_order;
    "map order", test_map_order;
    "max concurrency", test_max_concurrency;
    "filter", test_filter;
    "filter_map", test_filter_map;
    "exists", test_exists;
    "not exists", test_not_exists;
    "for_all", test_for_all;
    "not for_all", test_not_for_all;
  ]
end

let tests = Test.tests
