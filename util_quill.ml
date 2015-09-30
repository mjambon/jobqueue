(* 
 * This was built for the quilljs.com rich text editor.
 * It takes a quilljs Delta object (stored in JSON) and
 * converts it either to HTML or to Plaintext.
 *)
open Util_quill_t

type section =
| Ordered_list of section list
| Div_section of content list * string
| Li_section of content list * string
| Empty_section

let split_elem accum_list elem =
  let open Buffer in
  let buf = Buffer.create ((String.length elem.text) + 1) in
  let string_list = BatString.fold_left (fun l c ->
    add_char buf c;
    if c = '\n' then (
      let new_b = contents buf in
      reset buf;
      {text = new_b; attr = elem.attr} :: l
    ) else (
      l
    ))
    [] elem.text
  in
  let buf_text = contents buf in
  let string_list =
    if buf_text = "" then string_list
    else {text = buf_text; attr = elem.attr} :: string_list
  in
  string_list :: accum_list

let group_by_newline (result,temp) elem =
  if String.contains elem.text '\n' then
    ((List.rev(elem :: temp)) :: result, [])
  else (result, elem :: temp)

let group_by_list (l,temp) elem_list =
  let is_list,align = List.fold_right
    (fun x (b,s) -> if b then b,s else
      match x.attr with
      | None -> b,s
      | Some a ->
        (BatOption.default false a.list,BatOption.default "" a.align))
    elem_list (false,"")
  in
  if is_list then
    match temp with
    | Ordered_list (x) -> l, Ordered_list(x @ [Li_section(elem_list,align)])
    | _ -> l, Ordered_list([Li_section(elem_list,align)])
  else
    match temp with
    | Ordered_list (x) ->
      Div_section(elem_list,align) :: temp :: l, Empty_section
    | _ ->
      Div_section(elem_list,align) :: l, Empty_section

let create_span attribute value =
  let html_attribute = Util_html.encode attribute in
  let html_value = Util_html.encode value in
  "<span style=\"" ^ html_attribute ^ ": " ^ html_value ^ ";\">", "</span>"

let create_link address =
  let html_address = Util_html.encode address in
  "<a href=\"" ^ html_address ^ "\">", "</a>"

let create_div align =
  let html_align = Util_html.encode align in
  "<div style=\"text-align: " ^ html_align ^ ";\">"

let create_li align =
  let html_align = Util_html.encode align in
  "<li style=\"text-align: " ^ html_align ^ ";\">"

let get_elem html_buf elem =
  match elem.attr with
  | None ->  if Buffer.contents html_buf = "" && elem.text = "\n" then
               Buffer.add_string html_buf "<br>"
             else
               Buffer.add_string html_buf elem.text
  | Some a ->
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
  (*special case break if no other text*)
  let text = if Buffer.contents html_buf = "" && elem.text = "\n" then "<br>"
             else elem.text
  in
  Buffer.add_string html_buf (open_tags ^ text ^ close_tags)

let rec print_list html_buf = function
  | Ordered_list x ->
    Buffer.add_string html_buf "<ol>";
    List.iter (print_list html_buf) x;
    Buffer.add_string html_buf "</ol>"
  | Div_section (x,a) ->
    Buffer.add_string html_buf (create_div a);
    let buf_elem = Buffer.create 1000 in
    List.iter (get_elem buf_elem) x;
    Buffer.add_buffer html_buf buf_elem;
    Buffer.add_string html_buf "</div>"
  | Li_section (x,a) ->
    Buffer.add_string html_buf (create_li a);
    let buf_elem = Buffer.create 1000 in
    List.iter (get_elem html_buf) x;
    Buffer.add_buffer html_buf buf_elem;
    Buffer.add_string html_buf "</li>"
  | Empty_section -> Buffer.reset html_buf

(* Enter stringified json quill object, get html *)
let to_html json_string =
  let top = Util_quill_j.top_of_string json_string in
  match Util_quill_v.validate_top [] top with
  | Some x -> "Invalid note format."
  | None ->
  (* First separate elements by new line [a\nb; c\n] -> [a\n; b; c\n] *)
  let split_list = List.fold_left split_elem [] top.ops in
  let split_list = List.rev (List.concat split_list) in
  (* Combine elements based on new line [a\n; b; c\n] -> [[a\n]; [b; c\n]] *)
  let (combine_list,_) = List.fold_left group_by_newline ([],[]) split_list in
  let combine_list = List.rev combine_list in
  (* Combine sections based on list attribute *)
  let (final_list,extra) =
    List.fold_left group_by_list ([],Empty_section) combine_list
  in
  let final_list = if extra <> Empty_section then extra :: final_list
                   else final_list
  in
  let final_list = List.rev final_list in
  (* Print list *)
  let html_buf = Buffer.create 1000 in
  List.iter (print_list html_buf) final_list;
  Buffer.contents html_buf

(* Enter stringified JSON and get plaintext *)
let to_plaintext json_string =
  let top = Util_quill_j.top_of_string json_string in
  match Util_quill_v.validate_top [] top with
  | Some x -> "Invalid note format."
  | None ->
  List.fold_left (fun s o -> s ^ o.text) "" top.ops


let test_json = "{\"ops\":[{\"insert\":\"abcdef\"},{\"attributes\":\
{\"bold\":true},\"insert\":\"gef\"},{\"insert\":\"\\n\\n\\n\"},\
{\"attributes\":{\"underline\":true,\"bold\":true},\"insert\":\"what\"},\
{\"attributes\":{\"underline\":true},\"insert\":\"is\"},{\"attributes\":\
{\"underline\":true,\"italic\":true},\"insert\":\"this\"},\
{\"insert\":\"\\nlele\"},{\"attributes\":{\"list\":true},\"insert\":\"\\n\"},\
{\"insert\":\"duh\"},{\"attributes\":{\"list\":true,\"align\":\"center\"}\
,\"insert\":\"\\n\"}]}"

let plaintext_ans = "abcdefgef\n\n\nwhatisthis\nlele\nduh\n"

let html_ans = "<div style=\"text-align: ;\">abcdef<b>gef</b>\n</div>\
<div style=\"text-align: ;\"><br></div><div style=\"text-align: ;\"><br>\
</div><div style=\"text-align: ;\"><b><u>what</u></b><u>is</u><i><u>this</u>\
</i>\n</div><ol><li style=\"text-align: ;\">lele\n</li>\
<li style=\"text-align: center;\">duh\n</li></ol>"

let tests = [
  "Quill to Plaintext", (fun () -> plaintext_ans = (to_plaintext test_json));
  "Quill to HTML", (fun () -> html_ans = (to_html test_json))
]
