open Printf

let encode s =
  let buf = Buffer.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '&' -> Buffer.add_string buf "&amp;"
      | '\"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&apos;" (* valid in HTML5 only *)
      | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let paragraph_sep = Pcre.regexp "(?:\r*\n){2,}"
let line_sep = Pcre.regexp "\r*\n"

let split regexp s =
  Pcre.split ~rex:regexp s

let parse_text s =
  let paragraphs = split paragraph_sep s in
  BatList.map (split line_sep) paragraphs

let encode_paragraph l =
  "<p>" ^ String.concat "<br>\n" (BatList.map encode l) ^ "</p>"

let encode_text ll =
  String.concat "\n\n" (BatList.map encode_paragraph ll)

(* Turn plain text into HTML:
   Replace blank lines by paragraph separators,
   replace single line breaks by <br>.
   TODO: replace URLs by links
*)
let of_text s =
  encode_text (parse_text s)

let test_from_text () =
  let s = "\
Dear Santa,

I hope you're doing well
and I want a lot of presents.


Thanks!
"
  in
  let expected = "\
<p>Dear Santa,</p>

<p>I hope you&apos;re doing well<br>
and I want a lot of presents.</p>

<p>Thanks!</p>"
  in
  of_text s = expected

let tests = [
  "from text", test_from_text;
]
