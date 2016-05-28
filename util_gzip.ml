let try_finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let get_temp_dir () =
  let in_memory_dir = "/dev/shm" in
  if Sys.file_exists in_memory_dir then
    Some in_memory_dir
  else
    None

(*
   Apparently we have to write this ourselves.
*)
let decompress_string s =
  (* some arbitrary limit on input size *)
  if String.length s > 100_000_000 then
    invalid_arg "Util_gzip.decompress_string: input too large";
  let filename =
    let temp_dir = get_temp_dir () in
    Filename.temp_file ?temp_dir "wolverine-decompress-" ".gz" in
  let remove_file () = Sys.remove filename in
  try_finally
    (fun () ->
       BatPervasives.output_file ~filename ~text:s;
       let ic = Gzip.open_in filename in
       let close_file () = Gzip.close_in ic in
       try_finally
         (fun () ->
            let acc = Buffer.create (3 * String.length s) in
            let max_chunk_len = 8192 in
            let buf = Bytes.create max_chunk_len in
            let rec read_loop () =
              let n = Gzip.input ic buf 0 max_chunk_len in
              if n > 0 then (
                Buffer.add_substring acc buf 0 n;
                read_loop ()
              )
            in
            read_loop ();
            Buffer.contents acc
         )
         close_file
    )
    remove_file

let compress_string s =
  (* some arbitrary limit on input size *)
  if String.length s > 1_000_000_000 then
    invalid_arg "Util_gzip.compress_string: input too large";
  let temp_dir = get_temp_dir () in
  let filename = Filename.temp_file ?temp_dir "wolverine-compress-" ".gz" in
  let remove_file () = Sys.remove filename in
  try_finally
    (fun () ->
       let oc = Gzip.open_out filename in
       let close_file () = Gzip.close_out oc in
       try_finally
         (fun () ->
            Gzip.output oc s 0 (String.length s);
            BatPervasives.input_file ~bin:true filename
         )
         close_file
    )
    remove_file

let test_decompress_string () =
  let hex_input =
    "1f8b0808aa4fec56000368656c6c6f00f348cdc9c957e402009ed842b007000000"
  in
  let input = Util_hex.decode hex_input in
  let expected_output = "Hello!\n" in
  decompress_string input = expected_output

let test_compress_string () =
  let input = "Hello, world." in
  let z = compress_string input in
  let input' = decompress_string z in
  input = input'

let tests = [
  "decompress string", test_decompress_string;
  "compress string", test_compress_string;
]
