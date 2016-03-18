let gunzip_string s =
  let filename = Filename.temp_file "wolverine-" ".gz" in
  BatPervasives.output_file ~filename s;
  try
    let ic = Gzip.open_in filename in
    ...
  with e ->
    Sys.remove filename;
    Trax.raise __LOC__ e
