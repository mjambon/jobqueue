(*
  Warning: OCaml uses its own IDs for signals
*)
let ocaml_sig_list =
  let open Sys in
  [
    sigabrt, "SIGABRT", "Abnormal termination";
    sigalrm, "SIGALRM", "Timeout";
    sigfpe, "SIGFPE", "Arithmetic exception";
    sighup, "SIGHUP", "Hangup on controlling terminal";
    sigill, "SIGILL", "Invalid hardware instruction";
    sigint, "SIGINT", "Interactive interrupt (ctrl-C)";
    sigkill, "SIGKILL", "Termination (cannot be ignored)";
    sigpipe, "SIGPIPE", "Broken pipe";
    sigquit, "SIGQUIT", "Interactive termination";
    sigsegv, "SIGSEGV", "Invalid memory reference";
    sigterm, "SIGTERM", "Termination";
    sigusr1, "SIGUSR1", "Application-defined signal 1";
    sigusr2, "SIGUSR2", "Application-defined signal 2";
    sigchld, "SIGCHLD", "Child process terminated";
    sigcont, "SIGCONT", "Continue";
    sigstop, "SIGSTOP", "Stop";
    sigtstp, "SIGTSTP", "Interactive stop";
    sigttin, "SIGTTIN", "Terminal read from background process";
    sigttou, "SIGTTOU", "Terminal write from background process";
    sigvtalrm, "SIGVTALRM", "Timeout in virtual time";
    sigprof, "SIGPROF", "Profiling interrupt";
  ]

let ocaml_sig_tbl =
  let tbl = Hashtbl.create 30 in
  List.iter (fun (n, name, desc) ->
    Hashtbl.add tbl n (name, desc);
  ) ocaml_sig_list;
  tbl

let string_of_ocaml_signal n =
  match BatHashtbl.find_option ocaml_sig_tbl n with
      Some (name, desc) -> Printf.sprintf "%s (%s)" name desc
    | None -> Printf.sprintf "unknown signal (OCaml code %i)" n
