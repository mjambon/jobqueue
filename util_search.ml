(*
   Naive text search.
*)

open Printf

type query = string
  (* Words that must exist in the document, in any order. *)

(* Regexps that must match the document.
   whole_query or words must match. *)
type compiled_query = {
  whole_query: Pcre.regexp option;
  words: Pcre.regexp list option;
}

(*
   A case-insensitive match on an arbitrary string not followed or preceded
   by ascii word characters.
*)
let make_word_regexp w =
  let pattern =
    sprintf
      "(?<![A-Za-z_])%s(?![A-Za-z_])"
      (Pcre.quote w)
  in
  Pcre.regexp ~flags:[`CASELESS] pattern

let ascii_word_pattern = "[A-Za-z_]+"
let ascii_word_regexp = Pcre.regexp ascii_word_pattern

let extract_query_words s =
  try
    let aa = Pcre.extract_all ~rex:ascii_word_regexp s in
    let a =
      Array.map (function
        | [| sub |] -> sub
        | _ -> assert false
      ) aa
    in
    Array.to_list a
  with Not_found ->
    []

(*
   Compile a query given as a string by the end-user.
*)
let compile (query : query) : compiled_query =
  let query = Util_string.compact_whitespace query in
  let ascii_words = extract_query_words query in
  let norm_query = Util_string.ascii_lowercase query in
  let norm_words =
    Util_list.unique (BatList.map Util_string.ascii_lowercase ascii_words)
  in
  let whole_query =
    if not (BatList.mem norm_query norm_words) then
      Some (make_word_regexp norm_query)
    else
      None
  in
  let word_regexps =
    match norm_words with
    | [] -> None
    | l -> Some (BatList.map make_word_regexp l)
  in
  { whole_query; words = word_regexps }

(*
   Return whether a document matches the query.
*)
let matches compiled_query doc =
  (match compiled_query.whole_query with
   | None -> false
   | Some rex -> Pcre.pmatch ~rex doc
  )
  ||
  (match compiled_query.words with
   | None -> false
   | Some l ->
       BatList.for_all (fun rex -> Pcre.pmatch ~rex doc) l
  )

let test_search () =
  let yes query doc = matches (compile query) doc in
  let no query doc = not (yes query doc) in

  assert (no "" "abc");
  assert (yes "" "");
  assert (no "-" "a-b");
  assert (yes "-" "a - b");
  assert (yes "-" "-");
  assert (no "a-b" "--b");
  assert (yes "ab" "c-ab-c");
  assert (no "ab" "abc");
  assert (yes "ab cd" "cd-ab");
  assert (yes "ab-cd" "cd ab");
  assert (yes "dôme" "dôme");
  assert (yes "dôme" "meôd");
  assert (no "dôme" "domestic");

  true

let tests = [
  "search", test_search;
]
