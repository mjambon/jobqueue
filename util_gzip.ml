(*
   Apparently we have to write this ourselves.
*)
let gunzip_string s =
  (* some arbitrary limit on input size *)
  if String.length s > 100_000_000 then
    invalid_arg "Util_gzip.gunzip_string: input too large";
  let filename = Filename.temp_file "wolverine-" ".gz" in
  BatPervasives.output_file ~filename ~text:s;
  try
    let ic = Gzip.open_in filename in
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
    Sys.remove filename;
    Buffer.contents acc
  with e ->
    Sys.remove filename;
    Trax.raise __LOC__ e

let test_gunzip_string () =
  let hex_input =
    "1f8b0808aa4fec56000368656c6c6f00f348cdc9c957e402009ed842b007000000"
  in
  let input = Util_hex.decode hex_input in
  let expected_output = "Hello!\n" in
  gunzip_string input = expected_output
