(* Make ocaml-imap's Modseq module work with atdgen string wrap *)

include Imap.Imap_types.Gthrid
let wrap s = of_string s
let unwrap x = to_string x
