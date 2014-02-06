let encode s = Nlencoding.Url.encode ~plus:false  s
let decode s = Nlencoding.Url.decode ~plus:false s

(* Workaround incorrect Uri.path which does not encode the components.
   It works except for extra slashes inserted with %2F *)
let extract_path_elem url_string =
  let uri = Uri.of_string url_string in
  Pcre.split ~pat:"/" (Uri.path uri)

let extract_path url_string =
  let decoded_components = extract_path_elem url_string in
  String.concat "/" (BatList.map encode decoded_components)

let test_extract_path () =
  (* FIXME: "/a/b%2Fc" should give "/a/b%2Fc" instead of "/a/b/c" *)
  let input = "http://example.com/a/b%20c/d" in
  let expected_output = "/a/b%20c/d" in
  extract_path input = expected_output

let tests = [
  "extract path", test_extract_path;
]
