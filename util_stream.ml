
(* Streams created from line-formatted data files. *)


let pop st =
  match Stream.peek st with
      None -> None
    | Some _ as x ->
	Stream.junk st;
	x

let iteri f st =
  let counter = ref 0 in
  Stream.iter (fun x -> f !counter x; incr counter) st

(*
  Make a stream of the lines of an in_channel.
*)
let stream_of_channel ic =
  let next i =
    try Some (input_line ic)
    with End_of_file -> 
      close_in ic;
      None
  in
  Stream.from next

(*
  Make a stream of the lines of a file.
*)
let stream_of_file file =
  let ic = open_in file in
  let close_channel () = close_in_noerr ic in
  (stream_of_channel ic, close_channel)


(*
  1:1 mapping.
*)
let map f st =
  let next i =
    match pop st with
	None -> None
      | Some x -> Some (f x)
  in
  Stream.from next

let mapi f st =
  let next i =
    match pop st with
	None -> None
      | Some x -> Some (f i x)
  in
  Stream.from next

(*
  Filtering.
*)
let filter f st =
  let rec next i =
    let x = pop st in
    match x with
	None -> None
      | Some v ->
	  if f v then x
	  else next i
  in
  Stream.from next

(*
  map + filter
*)
let select f st =
  let rec next i =
    let x = pop st in
    match x with
	None -> None
      | Some v ->
	  match f v with
	      Some _ as y -> y
	    | None -> next i
  in
  Stream.from next

(*
  Discard non-matching elements of a stream until 
  one matching element is found.
*)
let skip_to f st =
  let skipping = ref true in

  let rec next i =
    if !skipping then 
      match pop st with
	  None -> None
	| Some v as x ->
	    if f v then (
	      skipping := false;
	      x
	    )
	    else
	      next i
    else
      pop st
  in
  Stream.from next

let really_skip_to ?(discard = fun _ -> ()) f st =
  let rec skip discard n f st =
    match Stream.peek st with
	None -> n
      | Some v ->
	  if not (f v) then (
	    Stream.junk st;
	    discard v;
	    skip discard (n + 1) f st
	  )
	  else
	    n
  in
  skip discard 0 f st


(*
  Map each element to a list and add to a single stream.
*)
let flatten f st =
  let q = Queue.create () in
  let rec next i =
    try Some (Queue.take q)
    with Queue.Empty ->
      match pop st with
	  None -> None
	| Some x ->
	    List.iter (fun x -> Queue.add x q) (f x);
	    next i
  in
  Stream.from next

(*
  Concatenate a list of streams.
*)
let concat l =
  match l with
      [] -> Stream.from (fun _ -> None)
    | first :: rest ->
	let q = Queue.create () in
	List.iter (fun x -> Queue.add x q) rest;
	let cur = ref first in
	let rec next i =
	  try Some (Stream.next !cur)
	  with Stream.Failure ->
	    if Queue.is_empty q then
	      None
	    else (
	      cur := Queue.take q;
	      next i
	    )
	in
	Stream.from next

(*
  Group by keys within a sorted list.
*)
let reduce cmp f st =
  let cur_key = ref None in
  let accu = ref [] in
  let rec next i =
    match !cur_key, pop st with
	Some k0, Some (k, data) -> 
	  if cmp k k0 = 0 then (
	    accu := data :: !accu;
	    next i
	  )
	  else (
	    let result = Some (f k0 (List.rev !accu)) in
	    cur_key := Some k;
	    accu := [data];
	    result
	  )

      | Some k0, None ->
	  let result = Some (f k0 (List.rev !accu)) in
	  cur_key := None;
	  accu := [];
	  result

      | None, Some (k, data) ->
	  cur_key := Some k;
	  accu := [data];
	  next i

      | None, None ->
	  None
  in
  Stream.from next


(*
  Merge streams of items sorted by keys.
*)
let merge cmp init fold l =
  match l with
      [] -> Stream.from (fun _ -> None)
    | l ->
	let next i =
	  let opt =
	    List.fold_left (
	      fun opt st ->
		match opt, Stream.peek st with
		    _, None -> opt
		  | Some key0, Some (key, _) ->
		      let c = cmp key0 key in
		      if c <= 0 then opt
		      else Some key
		  | None, Some (key, _) ->
		      Some key
	    ) None l
	  in
	  match opt with
	      None -> None
	    | Some key ->
		let accu = init key in
		let result =
		  List.fold_left (
		    fun accu st ->
		      match Stream.peek st with
			  Some (k, v) ->
			    if cmp key k = 0 then (
			      Stream.junk st;
			      fold accu v
			    )
			    else
			      accu
			| None ->
			    accu
		  ) accu l
		in
		Some (key, result)
	in
	Stream.from next
