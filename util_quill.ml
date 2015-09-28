(* 
 * This was built for the quilljs.com rich text editor.
 * It takes a quilljs Delta object (stored in JSON) and
 * converts it either to HTML or to Plaintext.
 *)
open Util_quill_j
open Util_quill_t
open Util_quill_v

let split_string elem = BatString.fold_left 
  (fun (s,l) c -> 
    if c = '\n' then ("",l @ [{text = (s ^ "\n"); attr = elem.attr}])
    else (s ^ (String.make 1 c),l))
  ("",[]) elem.text

let split_elem l elem =
  let (extra,first) = split_string elem in
  match extra, first with
  | "", [] -> l
  | "", y -> l @ y
  | x, [] -> l @ [{text = x; attr = elem.attr}]
  | x, y -> l @ y @ [{text = x; attr = elem.attr}]

let group_by_newline (result,temp) elem =
  if String.contains elem.text '\n' then (result @ [temp @ [elem]], [])
  else (result, temp @ [elem])

let create_span attribute value =
  let html_attribute = Util_html.encode attribute in
  let html_value = Util_html.encode value in
  "<span style=\"" ^ html_attribute ^ ": " ^ html_value ^ ";\">", "</span>"

let create_link address =
  let html_address = Util_html.encode address in
  "<a href=\"" ^ html_address ^ "\">", "</a>"

let create_div last_list align =
  let html_align = Util_html.encode align in
  let end_list = if last_list then "</ol>" else "" in
  end_list ^ "<div style=\"text-align: " ^ html_align ^ ";\">", "</div>"

let create_li last_list align =
  let html_align = Util_html.encode align in
  let start_list = if last_list then "" else "<ol>" in
  start_list ^ "<li style=\"text-align: " ^ html_align ^ ";\">", "</li>"

let get_elem (html, align, is_list) elem =
  match elem.attr with
  | None -> (html ^ elem.text,"",false)
  | Some a ->
  let list_info = BatOption.default false a.list in
  let align_info = BatOption.default "" a.align in
  let open_bold,close_bold =
  match a.bold with
  | None -> "",""
  | Some _ -> "<b>","</b>"
  in
  let open_italic,close_italic =
  match a.italic with
  | None -> "",""
  | Some _ -> "<i>","</i>"
  in
  let open_underline,close_underline =
  match a.underline with
  | None -> "",""
  | Some _ -> "<u>","</u>"
  in
  let open_color,close_color =
  match a.color with
  | None -> "",""
  | Some x -> create_span "color" x
  in
  let open_background,close_background =
  match a.background with
  | None -> "",""
  | Some x -> create_span "background-color" x
  in
  let open_font,close_font =
  match a.font with
  | None -> "",""
  | Some x -> create_span "font-family" x
  in
  let open_size,close_size =
  match a.size with
  | None -> "",""
  | Some x -> create_span "font-size" x
  in
  let open_link,close_link =
  match a.link with
  | None -> "",""
  | Some x -> create_link x
  in
  let open_tags = open_color ^ open_background ^ open_font ^ open_size
                  ^ open_link ^ open_bold ^ open_italic ^ open_underline
  in
  let close_tags = close_underline ^ close_italic ^ close_bold ^ close_link
                   ^ close_size ^ close_font ^ close_background ^ close_color
  in
  html ^ open_tags ^ elem.text ^ close_tags, align_info, list_info

let print_list (html,last_list) newline_section =
  let (elem_html, align, is_list) = 
    List.fold_left get_elem ("","",false) newline_section
  in
  let (open_tags, close_tags) =
    if is_list then create_li last_list align
    else create_div last_list align
  in
  html ^ open_tags ^ elem_html ^ close_tags, is_list

(* Enter stringified json quill object, get html *)
let to_html json_string =
  let top = Util_quill_j.top_of_string json_string in
  (* First separate elements by new line [a\nb; c\n] -> [a\n; b; c\n] *)
  let split_list = List.fold_left split_elem [] top.ops in
  (* Combine elements based on new line [a\n; b; c\n] -> [[a\n]; [b; c\n]] *)
  let (final_list,_) = List.fold_left group_by_newline ([],[]) split_list in
  (* Print list *)
  let (html,_) = List.fold_left print_list ("",false) final_list in
  html

(* Enter stringified JSON and get plaintext *)
let to_plaintext json_string =
  let top = Util_quill_j.top_of_string json_string in
  List.fold_left (fun s o -> s ^ o.text) "" top.ops


let test_json = "{\"ops\":[{\"attributes\":{\"color\":\"rgb(240, 102, 102)\
\",\"font\":\"serif\",\"size\":\"18px\",\"background\":\"rgb(187, 187, 187)\"\
,\"bold\":true,\"italic\":true,\"underline\":true},\"insert\"\
:\"this is a difficult test\"},{\"attributes\":{\"list\":true,\"align\"\
:\"center\"},\"insert\":\"\\n\"}]}"

let plaintext_ans = "this is a difficult test\n"

let html_ans = "<ol><li style=\"text-align: center;\">\
<span style=\"color: rgb(240, 102, 102);\">\
<span style=\"background-color: rgb(187, 187, 187);\">\
<span style=\"font-family: serif;\">\
<span style=\"font-size: 18px;\"><b><i><u>this is a difficult test\
</u></i></b></span></span></span></span>\n</li>"

let tests = [
  "Quill to Plaintext", (fun () -> plaintext_ans = (to_plaintext test_json));
  "Quill to HTML", (fun () -> html_ans = (to_html test_json))
]
