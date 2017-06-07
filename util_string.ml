(*
   String-manipulation utilities
*)

type t = string
let of_string s = s
let to_string s = s

(*
   Remove all ascii characters that are not a letter or a digit.
   Convert ascii letters to lowercase.
   Non-ascii bytes are preserved (only because we'd need a serious library
   to identify non-ascii letters, remove diacritical marks, and throw
   away the rest).

     "It's 2017!" -> "its2017"
*)
let ascii_alphanum s =
  let buf = Buffer.create (String.length s) in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
    | 'a'..'z' | '0'..'9' -> Buffer.add_char buf c
    | 'A'..'Z' -> Buffer.add_char buf (Char.chr (Char.code c + 32))
    | '\000'..'\127' (* remaining ascii characters *) -> ()
    | _ -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let test_ascii_alphanum () =
  assert (ascii_alphanum "" = "");
  assert (ascii_alphanum "aB, c 123 Å" = "abc123Å");
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
  "compact whitespace", test_compact_whitespace;
  "split prefix", test_split_prefix;
]
