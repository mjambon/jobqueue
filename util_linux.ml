open Printf
open Lwt

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

let ls dirname =
  Lwt_unix.opendir dirname >>= fun dir ->
  let rec read acc =
    let maxlen = 1025 in
    Lwt_unix.readdir_n dir maxlen >>= fun a ->
    let acc = List.rev_append (Array.to_list a) acc in
    if Array.length a < maxlen then
      return (List.rev acc)
    else
      read acc
  in
  Lwt.finalize
    (fun () -> read [])
    (fun () -> Lwt_unix.closedir dir)

let get_fd_count pid =
  let dirname = sprintf "/proc/%d/fd" pid in
  if Sys.file_exists dirname then
    ls dirname >>= fun l ->
    (* Exclude . and ..; assume all other files are decimal numbers
       identifying file descriptors *)
    return (Some (List.length l - 2))
  else
    return None

let test_file_descriptor_count () =
  ignore (get_fd_count (Unix.getpid ()));
  true

let tests = [
  "resident memory size", test_resident_memory_size;
  "file descriptor count", test_file_descriptor_count;
]
