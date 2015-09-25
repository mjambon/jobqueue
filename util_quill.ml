(* 
 * This was built for the quilljs.com rich text editor.
 * It takes a quilljs Delta object (stored in JSON) and
 * converts it either to HTML or to Plaintext.
 *)
open Yojson
open Yojson.Basic
open Yojson.Basic.Util

let cur_index = ref 0
let list_locations = ref []

let count_new_lines counter json =
  let text = json |> member "insert" |> to_string in
  let text_len = String.length text in
  let count = ref 0 in
  for i = 0 to text_len - 1 do
    if text.[i] = '\n' then count := !count + 1
  done;
  counter + !count

let create_span attribute value =
  let html_attribute = Util_html.encode attribute in
  let html_value = Util_html.encode value in
  "<span style=\"" ^ html_attribute ^ ": " ^ html_value ^ ";\">"

let create_link address =
  let html_address = Util_html.encode address in
  "<a href=\"" ^ html_address ^ "\">"

let create_div_align alignment =
  let html_alignment = Util_html.encode alignment in
  "<div style=\"text-align: " ^ html_alignment ^ ";\">"

let create_li_align alignment =
  let html_alignment = Util_html.encode alignment in
  "<li style=\"text-align: " ^ html_alignment ^ ";\">"

let check_location () =
  match !list_locations with
  | [] -> list_locations := (!cur_index, !cur_index) :: []
  | h::t ->
    match h with
    | x, y when y = !cur_index - 1 -> list_locations := (x, !cur_index) :: t
    | _, _ -> list_locations := (!cur_index, !cur_index) :: h :: t

let validate css_string =
  let regexColor = Str.regexp "^rgb([0-9]+, [0-9]+, [0-9]+)$" in
  let regexSize = Str.regexp "^[0-9]+px$" in
  match css_string with
  | "sans-serif" | "serif" | "monospace"
  | "left" | "center" | "right" -> css_string
  | x when (Str.string_match regexColor x 0) -> css_string
  | x when (Str.string_match regexSize x 0) -> css_string
  | _ -> ""

let parse_attributes json =
  match json with
  |`Null -> ("","")
  | _ ->
  let align = json |> member "align" |> to_string_option in
  let list = json |> member "list" |> to_bool_option in
  let font = json |> member "font" |> to_string_option in
  let link = json |> member "link" |> to_string_option in
  let bold = json |> member "bold" |> to_bool_option in
  let italic = json |> member "italic" |> to_bool_option in
  let underline = json |> member "underline" |> to_bool_option in
  let size = json |> member "size" |> to_string_option in
  let color = json |> member "color" |> to_string_option in
  let bg_color = json |> member "background" |> to_string_option in

  let close_tags = ref "" in
  let close_special_tags = ref "" in
  let list_html =
    match list with
    | None -> ""
    | Some _ -> close_tags := ("</li>" ^ !close_special_tags);
                check_location ();
                match align with
                | None -> "<li>"
                | Some y -> create_li_align y
  in
  let align_html =
    match align with
    | None -> ""
    | Some x -> if list_html = "" then (
                  close_tags := ("</div>" ^ !close_special_tags);
                  create_div_align x;
                ) else ""
  in
  let font_html =
    match font with
    | None -> ""
    | Some x -> close_tags := ("</span>" ^ !close_tags);
                              create_span "font-family" (validate x)
  in
  let size_html =
    match size with
    | None -> ""
    | Some x -> close_tags := ("</span>" ^ !close_tags);
                              create_span "font-size" (validate x)
  in
  let color_html =
    match color with
    | None -> ""
    | Some x -> close_tags := ("</span>" ^ !close_tags);
                              create_span "color" (validate x)
  in
  let bg_color_html =
    match bg_color with
    | None -> ""
    | Some x -> close_tags := ("</span>" ^ !close_tags);
                              create_span "background-color" (validate x)
  in
  let link_html =
    match link with
    | None -> ""
    | Some x -> close_tags := ("</a>" ^ !close_tags); create_link x
  in
  let bold_html =
    match bold with
    | None -> ""
    | Some _ -> close_tags := ("</b>" ^ !close_tags);"<b>"
  in
  let italic_html =
    match italic with
    | None -> ""
    | Some _ -> close_tags := ("</i>" ^ !close_tags);"<i>"
  in
  let underline_html =
    match underline with
    | None -> ""
    | Some _ -> close_tags := ("</u>" ^ !close_tags);"<u>"
  in
  let attr_tags = 
    if list_html <> "" || align_html <> "" then list_html ^ align_html
    else
      font_html ^ size_html ^ color_html ^ bg_color_html ^
      link_html ^ bold_html ^ italic_html ^ underline_html
  in
  (
    attr_tags,
    if !close_special_tags = "" then !close_tags else !close_special_tags
  )

let apply_attributes final_array attr_tags close_tags new_text newline =
  let cur_text = final_array.(!cur_index) in
  if newline then (
    if new_text = "\n" then (
      if final_array.(!cur_index) = "" then (
        if close_tags <> "" then 
          final_array.(!cur_index) <- (attr_tags ^ "<br>" ^ close_tags)
        else
          final_array.(!cur_index) <- ("<div><br></div>")
      ) else (
        if close_tags <> "" then
          final_array.(!cur_index) <- (attr_tags ^ cur_text ^ close_tags)
        else
          final_array.(!cur_index) <- ("<div>" ^ cur_text ^ "</div>")
      )
    ) else (
      final_array.(!cur_index) 
        <- "<div>" ^ cur_text ^ attr_tags ^ new_text ^ close_tags ^ "</div>"
    )
  ) else (
    final_array.(!cur_index) <- cur_text ^ attr_tags ^ new_text ^ close_tags
  )

let parse_list final_array json =
  let attributes_json = json |> member "attributes" in
  let (attr_tags, close_tags) = parse_attributes attributes_json in
  let text = json |> member "insert" |> to_string in
  let text_len = String.length text in
  let new_text = ref "" in

  for i=0 to text_len - 1 do
    new_text := !new_text ^ (String.make 1 text.[i]);
    if text.[i] = '\n' then (
      apply_attributes final_array attr_tags close_tags !new_text true;
      new_text := "";
      cur_index := !cur_index + 1;
    ) else if i = text_len-1 then (
      apply_attributes final_array attr_tags close_tags !new_text false;
    )
  done

let print_array size final_array =
  let buffer = ref "" in
  for i=0 to size-1 do
    let line = ref "" in
    let list_start = List.fold_left (fun x (f,_) -> if f = i then true else x)
                                    false !list_locations
    in
    (if list_start then line := "<ol>" ^ !line);
    line := !line ^ final_array.(i);
    let list_end = List.fold_left (fun x (_,s) -> if s = i then true else x)
                                  false !list_locations
    in
    (if list_end then line := !line ^ "</ol>");
    buffer := !buffer ^ !line;
  done;
  !buffer

(* Enter a strigified JSON, and get HTML *)
let to_html json_string =
  try
    cur_index := 0;
    list_locations := [];
    let json = from_string json_string in
    let list_all = json |> member "ops" |> to_list in
    let find_size = List.fold_left count_new_lines 0 list_all in
    let final_array = Array.init find_size (fun _ -> "") in
    List.iter (parse_list final_array) list_all;
    (*List.iter (fun (x,y) -> print_int x; print_string ","; print_int y; print
    _string "\n") !list_locations;*)
    print_array find_size final_array
  with
  | _ -> "Could not retrieve notes."


let get_text accum json =
  let text = json |> member "insert" |> to_string in
  accum ^ text

(* Enter stringified JSON and get plaintext *)
let to_plaintext json_string =
  try
    let json = from_string json_string in
    let list_all = json |> member "ops" |> to_list in
    List.fold_left get_text "" list_all
  with
  | _ -> "Could not retrieve notes."


let test_json = "{\"ops\":[{\"attributes\":{\"color\":\"rgb(240, 102, 102)\",\"font\":\"serif\",\"size\":\"18px\",\"background\":\"rgb(187, 187, 187)\",\"bold\":true,\"italic\":true,\"underline\":true},\"insert\":\"this is a difficult test\"},{\"attributes\":{\"list\":true,\"align\":\"center\"},\"insert\":\"\\n\"}]}"
let plaintext_ans = "this is a difficult test\n"
let html_ans = "<ol><li style=\"text-align: center;\"><span style=\"font-family: serif;\"><span style=\"font-size: 18px;\"><span style=\"color: rgb(240, 102, 102);\"><span style=\"background-color: rgb(187, 187, 187);\"><b><i><u>this is a difficult test</u></i></b></span></span></span></span></li></ol>"
let tests = [
  "Quill to Plaintext", (fun () -> plaintext_ans = (to_plaintext test_json));
  "Quill to HTML", (fun () -> html_ans = (to_html test_json))
]
