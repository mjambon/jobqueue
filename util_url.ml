module Op = struct
  (* Add an optional (k, v) pair to the query *)
  let (@^@) (k, f, opt_v) query =
    match opt_v with
    | None   -> query
    | Some v -> (k, [f v]) :: query

  (* Add multiple optional (k, v) pairs to the query, repeating the same key *)
  let (@^^@) (k, f, opt_l) query =
    match opt_l with
    | None   -> query
    | Some l -> BatList.fold_right (fun v q -> (k, [f v]) :: q) l query
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
