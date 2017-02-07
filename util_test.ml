open Printf

let important_line_prefix = "| "

let print_running name =
  eprintf "%s%-30s %s\n%!"
    important_line_prefix name "..."

let print_ended name =
  eprintf "%s%-30s %s\n%!"
    important_line_prefix name "ended."

let print_outcome name success =
  let status_msg =
    match success with
    | true -> "OK"
    | false -> "ERROR"
  in
  eprintf "%s%-30s %s\n%!"
    important_line_prefix name status_msg

let run_test (k, f) =
  flush stdout;
  flush stderr;
  let success =
    try
      let success, data = f () in
      success, Some data
    with e ->
      eprintf "Uncaught exception: %s\n" (Trax.to_string e);
      false, None
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

(*
   Run a list of tests.
*)
let run test_suite_name (l : (string * (unit -> bool * 'a)) list) =
  let results = List.map run_test l in
  List.iter (fun (name, (success, opt)) ->
    print_outcome name success
  ) results;
  let n = List.length results in
  let successes =
    List.length
      (BatList.filter (fun (k, (success, opt)) -> success) results)
  in
  eprintf "Passed %i/%i %s\n%!" successes n test_suite_name;
  let total_success = (successes = n) in
  total_success, results

let simple_run test_suite_name (l : (string * (unit -> bool)) list) : bool =
  let total_success, data =
    run test_suite_name (
      List.map (fun (name, f) ->
        let g () = f (), None in
        (name, g)
      ) l
    )
  in
  total_success
