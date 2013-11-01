exception Y of float

(* Linear interpolation *)
let interpolate ?ymin ?ymax a x =
  if Array.length a < 1 then
    invalid_arg "Util_model.interpolate";
  let ymin = match ymin with None -> snd a.(0) | Some y -> y in
  let ymax = match ymax with None -> snd a.(Array.length a - 1) | Some y -> y in
  try
    for i = 0 to Array.length a - 2 do
      let j = i + 1 in
      let x1, y1 = a.(i) in
      let x2, y2 = a.(j) in
      assert (x2 > x1);
      if x < x1 then
        raise (Y ymin)
      else if x <= x2 then (
        let r = (y2 -. y1) /. (x2 -. x1) in
        let y = y1 +. r *. (x -. x1) in
        raise (Y y)
      )
    done;
    ymax

  with Y y -> y

let test_interpolate () =
  let a =
    [|
      -1., -1.;
      0., 10.;
      0.5, -3.;
      1.1, 0.2;
      1.5, 1000.;
      2.0, 0.;
      10., 0.;
      12345., 1.;
    |]
  in
  let ymin = 0. in
  let ymax = infinity in
  List.map (fun x -> x, interpolate ~ymin ~ymax a x)
    [ -2.; 0.; 0.1; 1.0; 1.4; 1.95; 7.5; 11.; 10000.; 1e6 ]
