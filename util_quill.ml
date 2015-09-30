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

let split_string elem =
  let open Buffer in
  let buf = Buffer.create ((String.length elem.text) + 1) in
  BatString.fold_left (fun (l,b) c ->
    if c = '\n' then (
      let new_b = add_char b c; contents b in
      l @ [{text = new_b; attr = elem.attr}], (reset b; b)
    ) else (
      l, (add_char b c; b)
    ))
  ([],buf) elem.text

let split_elem l elem =
  let open Buffer in
  let (first,extra) = split_string elem in
  match contents extra, first with
  | "", [] -> l
  | "", y -> l @ y
  | x, [] -> l @ [{text = x; attr = elem.attr}]
  | x, y -> l @ y @ [{text = x; attr = elem.attr}]

let group_by_newline (result,temp) elem =
  if String.contains elem.text '\n' then (result @ [temp @ [elem]], [])
  else (result, temp @ [elem])

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
      l @ [temp] @ [Div_section(elem_list,align)], Empty_section
    | _ ->
      l @ [Div_section(elem_list,align)], Empty_section

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
  (* Combine elements based on new line [a\n; b; c\n] -> [[a\n]; [b; c\n]] *)
  let (combine_list,_) = List.fold_left group_by_newline ([],[]) split_list in
  (* Combine sections based on list attribute *)
  let (result,extra) =
    List.fold_left group_by_list ([],Empty_section) combine_list
  in
  let final_list =
    match extra with
    | Ordered_list x -> result @ [extra]
    | _ -> result
  in
  (* Print list *)
  let html_buf = Buffer.create 1000 in
  List.iter (print_list html_buf) final_list;
  Buffer.contents html_buf

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
</u></i></b></span></span></span></span>\n</li></ol>"

let tests = [
  "Quill to Plaintext", (fun () -> plaintext_ans = (to_plaintext test_json));
  "Quill to HTML", (fun () -> html_ans = (to_html test_json))
]
