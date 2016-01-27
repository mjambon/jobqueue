open Printf

let split_proc_status_line s =
  try
    let k, v = BatString.split s ~by:":" in
    k, BatString.trim v
  with Not_found ->
    "", s

let parse_proc_status_bytes s =
  try
    let num, unit = BatString.split s ~by:" " in
    let n = float_of_string num in
    match unit with
    | "B" -> n
    | "kB" | "KB" -> 2.**10. *. n
    | "mB" | "MB" -> 2.**20. *. n
    | "gB" | "GB" -> 2.**30. *. n
    | "tB" | "TB" -> 2.**40. *. n
    | _ -> raise Exit
  with _ ->
    failwith ("Cannot parse /proc/PID/status value: " ^ s)

let parse_proc_status fname =
  let lines = BatList.of_enum (BatFile.lines_of fname) in
  let kv_list = BatList.map split_proc_status_line lines in
  kv_list

let get_resident_memory_size pid =
  let fname = sprintf "/proc/%d/status" pid in
  if Sys.file_exists fname then
    let kv_list = parse_proc_status fname in
    let v = List.assoc "VmRSS" kv_list in
    Some (parse_proc_status_bytes v)
  else
    None

let test_resident_memory_size () =
  ignore (get_resident_memory_size (Unix.getpid ()));
  true

let tests = [
  "resident memory size", test_resident_memory_size;
]
