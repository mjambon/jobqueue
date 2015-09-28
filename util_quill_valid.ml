open Util_quill_t

let regex_color = Str.regexp "^rgb([0-9]+, [0-9]+, [0-9]+)$"
let regex_size = Str.regexp "^[0-9]+px$"

let validate attr =
  let test_font = match attr.font with
  | Some "sans-serif" | Some "serif" | Some "monospace" -> true
  | None -> true
  | _ -> false
  in
  let test_align = match attr.align with
  | Some "left" | Some "center" | Some "right" -> true
  | None -> true
  | _ -> false
  in
  let test_color = match attr.color with
  | Some x when (Str.string_match regex_color x 0) -> true
  | None -> true
  | _ -> false
  in
  let test_background = match attr.background with
  | Some x when (Str.string_match regex_color x 0) -> true
  | None -> true
  | _ -> false
  in
  let test_size = match attr.size with
  | Some x when (Str.string_match regex_size x 0) -> true
  | None -> true
  | _ -> false
  in
  test_font && test_align && test_color && test_background && test_size
