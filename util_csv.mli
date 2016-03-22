type ('a,'b) columns

val col: header:string -> ('a->string) -> ('a->'b,'b) columns
val (^^): ('a,'b) columns -> ('b,'c) columns -> ('a,'c) columns

val headers: ('a,'b) columns -> string list

val make_row: ('row_maker, string list) columns -> 'row_maker
(* 'row_maker is a function taking arguments for each columns, returning
   a string list. *)

val with_stream_channel: (Csv.out_channel -> unit Lwt.t) -> char Lwt_stream.t

val of_string : string -> Csv.t
val to_string : Csv.t -> string
