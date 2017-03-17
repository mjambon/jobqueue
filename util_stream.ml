(*
   General-purpose stream functions
*)

let fold_left f acc0 st =
  let acc = ref acc0 in
  Stream.iter (fun x -> acc := f !acc x) st;
  !acc

let to_list_rev st =
  fold_left (fun l x -> x :: l) [] st

let to_list st =
  List.rev (to_list_rev st)

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
let merge_full cmp init fold l =
  let next i =
    (* Find the smallest key at the beginning of each stream
       without consuming them. *)
    let opt =
      List.fold_left (
        fun opt st ->
          match opt, Stream.peek st with
          | _, None -> opt
          | Some key0, Some (key, _) ->
              let c = cmp key0 key in
              if c <= 0 then opt
              else Some key
          | None, Some (key, _) ->
              Some key
      ) None l
    in
    match opt with
    | None -> None
    | Some key ->
        (* Initialize an accumulator for the new key *)
        let acc = init key in
        (* Read all the values that have this same key at the beginning
           of each stream. *)
        let result =
          List.fold_left (
            fun acc st ->
              let rec loop acc =
                match Stream.peek st with
                | Some (k, v) ->
                    if cmp key k = 0 then (
                      (* Consume the stream and add value to accumulator. *)
                      Stream.junk st;
                      let acc = fold acc v in
                      (* Look for more of the same key
                         within the same stream. *)
                      loop acc
                    )
                    else
                      acc
                | None ->
                    acc
              in
              loop acc
          ) acc l
        in
        Some (key, result)
  in
  match l with
  | [] ->
      Stream.from (fun _ -> None)
  | l ->
      Stream.from next

(*
   Simplified version of merge_full which flattens the lists of values
   found under the same key.
*)
let merge cmp streams =
  let kv_streams =
    List.rev (
      List.rev_map (fun stream ->
        map (fun x -> (x, x)) stream
      ) streams
    )
  in
  let grouped_by_key =
    merge_full
      cmp
      (fun k -> [])
      (fun acc v -> v :: acc)
      kv_streams
  in
  flatten (fun (k, vl) -> vl) grouped_by_key

let test_merge () =
  let merge ll =
    let streams = List.map Stream.of_list ll in
    to_list (merge compare streams)
  in
  assert (merge [] = []);
  assert (merge [[]] = []);
  assert (merge [[]; []; []] = []);
  assert (merge [[1; 2; 3]; [4; 5]] = [1; 2; 3; 4; 5]);
  assert (merge [[1; 3]; [2; 4; 5]] = [1; 2; 3; 4; 5]);
  assert (merge [[2; 4; 5]; [1; 3]] = [1; 2; 3; 4; 5]);
  assert (merge [[1; 3; 6]; [4; 5]; [2; 7]] = [1; 2; 3; 4; 5; 6; 7]);
  assert (merge [[2; 4; 4; 5]; [1; 2; 3]] = [1; 2; 2; 3; 4; 4; 5]);
  true

let test_merge_full () =
  let deduplicate ll =
    let streams =
      List.map (fun l ->
        let kvl =
          List.map (fun x -> (x, ())) l
        in
        Stream.of_list kvl
      ) ll
    in
    let l =
      to_list (
        merge_full
          compare
          (fun k -> ())
          (fun acc () -> ())
          streams
      )
    in
    List.map (fun (k, ()) -> k) l
  in
  assert (deduplicate [] = []);
  assert (deduplicate [[1; 3; 6]; [4; 5]; [2; 7]] = [1; 2; 3; 4; 5; 6; 7]);
  assert (deduplicate [[2; 4; 4; 5]; [1; 2; 3]] = [1; 2; 3; 4; 5]);
  true

let tests = [
  "merge", test_merge;
  "merge_full", test_merge_full;
]
