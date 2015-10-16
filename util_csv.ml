type ('a, 'b)columns = {
  headers: string list;
  make_row: (string list->'b)->'a;
}

let col ~header string_of = {
  headers = [header];
  make_row = fun return x -> return [string_of x];
}

let (^^) x y = {
  headers = x.headers @ y.headers;
  make_row = fun return -> x.make_row (fun x_row ->
                           y.make_row (fun y_row ->
                           return (x_row @ y_row)));
}

let headers cols =
  cols.headers

let make_row cols =
  cols.make_row (fun row -> row)

let with_stream_channel f =
  let stream, push = Lwt_stream.create () in
  let channel = object
    method output bytes ofs len =
      for i = ofs to ofs+len-1 do
        push (Some (Bytes.get bytes i))
      done;
      len
    method close_out () =
      push None
  end in
  Lwt.async (fun () ->
    Lwt.finalize
      (fun () -> f (Csv.to_out_obj channel))
      (fun () -> Lwt.return (push None)));
  stream
