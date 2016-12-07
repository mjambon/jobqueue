type t = string
let of_string s = s
let to_string s = s

let ascii_lowercase s =
  let s' = Bytes.of_string s in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | 'A'..'Z' as c -> Bytes.set s' i (Char.chr (Char.code c + 32))
    | _ -> ()
  done;
  Bytes.to_string s'

let test_ascii_lowercase () =
  ascii_lowercase "AbCd\xc9F" = "abcd\xc9f"

let ascii_capitalize s =
  match s with
  | "" -> s
  | _ ->
      match s.[0] with
      | 'a'..'z' -> String.capitalize s
      | _ -> s

let test_ascii_capitalize () =
  assert (String.capitalize "\xe0" <> "\xe0");
  assert (ascii_capitalize "\xe0" = "\xe0");
  assert (ascii_capitalize "ab" = "Ab");
  true

let whitespace_rex = Pcre.regexp "[ \t\r\n]+"

let compact_whitespace s =
  let s = BatString.strip ~chars:" \t\r\n" s in
  Pcre.substitute
    ~rex: whitespace_rex
    ~subst: (fun _ -> " ")
    s

let test_compact_whitespace () =
  compact_whitespace " \ta bb \t \n\n ccc\n\t" = "a bb ccc"

let ascii_normalize s =
  compact_whitespace (ascii_lowercase s)

(*
   Separate prefix from what follows, based on a single character separator:

     sep = ':'
     "foo:bar" -> ("foo", "bar")
     "bar"     -> ("", "bar")
*)
let split_prefix ~sep s =
  try
    let i = String.index s sep in
    let len = String.length s in
    (String.sub s 0 i, String.sub s (i + 1) (len - i - 1))
  with Not_found ->
    ("", s)

let test_split_prefix () =
  assert (split_prefix ~sep:':' "" = ("", ""));
  assert (split_prefix ~sep:':' "a" = ("", "a"));
  assert (split_prefix ~sep:':' "a:b" = ("a", "b"));
  assert (split_prefix ~sep:':' ":b" = ("", "b"));
  assert (split_prefix ~sep:':' "a:" = ("a", ""));
  assert (split_prefix ~sep:':' "ab:cde" = ("ab", "cde"));
  true

let tests = [
  "ascii lowercase", test_ascii_lowercase;
  "ascii capitalize", test_ascii_capitalize;
  "compact whitespace", test_compact_whitespace;
  "split prefix", test_split_prefix;
]
