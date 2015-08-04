module Op = struct
  let (@^@) (k,f,v) query =
    match v with
    | None   -> query
    | Some v -> (k, [f v]) :: query
end

let encode s = Nlencoding.Url.encode ~plus:false s
let decode s = Nlencoding.Url.decode ~plus:false s

let extract_path url_string =
  Uri.path (Uri.of_string url_string)

let test1_extract_path () =
  let input = "/a/b%2Fc" in
  let expected_output = "/a/b%2Fc" in
  extract_path input = expected_output

let test2_extract_path () =
  let input = "http://example.com/a/b%20c/d" in
  let expected_output = "/a/b%20c/d" in
  extract_path input = expected_output

let tests = [
  "extract path 1", test1_extract_path;
  "extract path 2", test2_extract_path;
]
