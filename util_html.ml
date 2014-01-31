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

let split_lines s =
  split line_sep s

let parse_paragraphs s =
  let paragraphs = split paragraph_sep s in
  BatList.map split_lines paragraphs

type with_quotes =
  | Paragraphs of string list list
  | Quote of with_quotes list

(* group contiguous elements that have the same (boolean) property *)
let map_contiguous cond f g l =
  let rec aux acc acc2 prop l =
    let func = if prop then f else g in
    match l with
    | [] ->
        let acc =
          if acc2 = [] then acc
          else func (List.rev acc2) :: acc
        in
        List.rev acc
    | x :: tl ->
        if cond x = prop then
          aux acc (x :: acc2) prop tl
        else
          let acc =
            if acc2 = [] then acc
            else func (List.rev acc2) :: acc
          in
          aux acc [x] (not prop) tl
  in
  aux [] [] true l

let test_map_contiguous () =
  let is_even x = x mod 2 = 0 in
  let even_group l = `Even l in
  let odd_group l = `Odd l in
  let test_one input expected_output =
    let output = map_contiguous is_even even_group odd_group input in
    expected_output = output
  in
  List.for_all (fun (input, expected_output) ->
    test_one input expected_output
  )
    [
      [], [];
      [1], [`Odd [1]];
      [1;3], [`Odd [1;3]];
      [0], [`Even [0]];
      [1;3;0;2;7], [`Odd [1;3]; `Even [0;2]; `Odd [7]];
    ]

let has_quote_prefix line =
  line <> "" && line.[0] = '>'

let has_space_prefix line =
  line = "" || line.[0] = ' '

let remove_first_character line =
  if line = "" then ""
  else String.sub line 1 (String.length line - 1)

let rec remove_leading_spaces_evenly lines =
  if List.for_all has_space_prefix lines then
    remove_leading_spaces_evenly (BatList.map remove_first_character lines)
  else
    lines

let rec parse_quotes lines =
  map_contiguous
    has_quote_prefix
    (fun quoted_lines ->
       let lines = BatList.map remove_first_character quoted_lines in
       let lines = remove_leading_spaces_evenly lines in
       Quote (parse_quotes lines)
    )
    (fun regular_lines ->
       let s = String.concat "\n" regular_lines in
       let paragraphs = parse_paragraphs s in
       Paragraphs paragraphs
    )
    lines

let encode_paragraph l =
  "<p>" ^ String.concat "<br>\n" (BatList.map encode l) ^ "</p>\n"

let encode_paragraphs ll =
  String.concat "\n\n" (BatList.map encode_paragraph ll)

let encode_text l =
  let rec print_one buf = function
    | Quote l ->
        bprintf buf
          "<blockquote type=\"cite\">\n%a\n</blockquote>\n"
          print_list l
    | Paragraphs l ->
        Buffer.add_string buf (encode_paragraphs l)

  and print_list buf l =
    List.iter (print_one buf) l
  in
  let buf = Buffer.create 1000 in
  print_list buf l;
  Buffer.contents buf

(* Turn plain text into HTML:
   Replace blank lines by paragraph separators,
   replace single line breaks by <br>.
   TODO: replace URLs by links
*)
let of_text s =
  encode_text (parse_quotes (split_lines s))

let test_from_text () =
  let s = "\
Dear Santa,

I hope you're doing well
and I want a lot of presents.


Thanks!

> What would you like this year?
> 
> -- Santa Claus
> 
> >Hey, are you
> >the real Santa?

>      The following is indented but it won't be rendered as such:
>                           ***
"
  in
  let expected = "\
<p>Dear Santa,</p>


<p>I hope you&apos;re doing well<br>
and I want a lot of presents.</p>


<p>Thanks!</p>
<blockquote type=\"cite\">
<p>What would you like this year?</p>


<p>-- Santa Claus</p>
<blockquote type=\"cite\">
<p>Hey, are you<br>
the real Santa?</p>

</blockquote>

</blockquote>
<blockquote type=\"cite\">
<p>The following is indented but it won&apos;t be rendered as such:<br>
                     ***</p>

</blockquote>
"
  in
  of_text s = expected

let tests = [
  "from text", test_from_text;
]
