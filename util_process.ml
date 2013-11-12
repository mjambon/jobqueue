(*
   Submit data to a process via stdin and read the responses back
   from stdout one line at a time,
   keeping the process alive for as long as possible.
*)

open Printf
open Lwt
open Log

let seq a b =
  catch
    (fun () -> a >>= fun _ -> return ())
    (fun e -> return ())
  >>= fun () ->
  b ()

let init cmd =
  let worker = ref None in
  let last = ref (return ()) in
  let get_new_worker () =
    let x = Lwt_process.open_process cmd in
    worker := Some x;
    x
  in
  let get_worker () =
    match !worker with
    | None -> get_new_worker ()
    | Some x -> x
  in
  let cleanup () =
    match !worker with
    | Some p ->
        p # close >>= fun status ->
        worker := None;
        last := return ();
        return ()
    | None ->
        return ()
  in
  let submit data =
    let p = get_worker () in
    catch
      (fun () ->
         let result_t =
           seq !last (fun () ->
             let oc = p#stdin in
             Lwt_io.write_line oc data >>= fun () ->
             Lwt_io.flush oc >>= fun () ->
             Lwt_io.read_line p#stdout
           )
         in
         last := (result_t >>= fun _ -> return ());
         result_t
      )
      (function e ->
         eprintf "Command %s caused the following error: %s%s\n%!"
           (fst cmd)
           (Printexc.to_string e)
           (if e = End_of_file then
              sprintf " (is %s in $PATH?)" (fst cmd)
            else "");
         cleanup () >>= fun () ->
         return ""
      )
  in
  submit

let test_cat () =
  let submit = init ("cat", [| "cat" |]) in
  let s = "Howdy!" in
  let s' = Lwt_main.run (submit s) in
  s = s'

let tests = [
  "cat", test_cat;
]
