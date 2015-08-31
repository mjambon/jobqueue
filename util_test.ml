open Printf

let run_test (k, f) =
  flush stdout;
  flush stderr;
  let success =
    try f ()
    with e ->
      eprintf "Uncaught exception: %s\n" (Trax.to_string e);
      false
  in
  (k, success)

let flatten tests =
  List.flatten (
    List.map (
      fun (section, l) ->
        List.map
          (fun (k, f) -> (sprintf "%s> %s" section k, f))
          l
    ) tests
  )

let print_error (k, success) =
  if not success then
    eprintf "FAILED %s\n" k

(*
   Run a list of tests.
*)
let run (l : (string * (unit -> bool)) list) =
  let results = List.map run_test l in
  List.iter print_error results;
  let n = List.length results in
  let successes =
    List.fold_left
      (fun acc (k, success) -> if success then acc + 1 else acc) 0 results
  in
  eprintf "Passed %i/%i tests\n%!" successes n;
  (successes = n)

