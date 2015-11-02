(*
   Read a process ID from a file.
   If the file exists, it must contain just an int.
*)
let get_previous_worker_pid pid_file =
  if Sys.file_exists pid_file then
    let pid_string = BatPervasives.input_file pid_file in
    Some (int_of_string pid_string)
  else
    None

let save_pid pid_file =
  BatPervasives.output_file
    ~filename: pid_file
    ~text: (string_of_int (Unix.getpid()))

let process_is_killable pid =
  try
    Unix.kill pid 0;
    true
  with _ ->
    (*
       Process is not killable because it doesn't exist or because it is
       owned by another user.
    *)
    false

let previous_worker_is_still_running pid_file =
  match get_previous_worker_pid pid_file with
  | None -> false
  | Some pid -> process_is_killable pid
