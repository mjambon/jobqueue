open Printf

let encode s =
  let buf = Buffer.create (2 * String.length s) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '&' -> Buffer.add_string buf "&amp;"
      | '\"' -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf
