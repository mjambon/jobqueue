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

let tests = [
  "ascii lowercase", test_ascii_lowercase;
  "compact whitespace", test_compact_whitespace;
]
