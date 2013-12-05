(* Geographic coordinates in degrees *)
type latlon = {
  lat: float;
  lon: float
}

let string_of_latlon latlon =
  string_of_float latlon.lat ^ "," ^ string_of_float latlon.lon

let latlon_of_string s =
  Scanf.sscanf s "%f,%f" (fun lat lon -> {lat; lon})

let approximate_string_of_latlon_time (latlon, time) =
  Printf.sprintf "%.2f,%.2f%@%f" latlon.lat latlon.lon time

let latlon_time_of_string s =
  Scanf.sscanf s "%f,%f%@%f" (fun lat lon time -> ({lat; lon}, time))

(* Very simple spherical distance implementation
   Assumes Earth has uniform radius of ~6371 kilometers
   Uses Haversine formula *)

let pi = acos (-. 1.)

let radians deg =
  deg *. pi /. 180.

let degrees rad =
  180. *. rad /. pi

let earth_radius = 6371000. (* meters *)

let haversine latlon1 latlon2 =
  let lat1 = latlon1.lat and lat2 = latlon2.lat in
  let lon1 = latlon1.lon and lon2 = latlon2.lon in
  let d_lat = radians (lat2 -. lat1) in
  let d_lon = radians (lon2 -. lon1) in
  let lat_sin = sin (d_lat /. 2.) in
  let lon_sin = sin (d_lon /. 2.) in
  let a =
    lat_sin *. lat_sin
    +. (cos (radians lat1)) *. (cos (radians lat2))
    *. lon_sin *. lon_sin
  in
  let c = 2. *. asin (sqrt a) in
  earth_radius *. c

let arc_length loc1 loc2 =
  if loc1 = loc2 then 0. else haversine loc1 loc2

let arc_length_opt opt_loc1 opt_loc2 =
  match opt_loc1, opt_loc2 with
  | None, _
  | _, None -> 0.
  | Some loc1, Some loc2 -> arc_length loc1 loc2

let palo_alto = { lat = 37.429167; lon = -122.138056 }
let san_francisco = { lat = 37.7833; lon = -122.4167 }
let new_york_city = { lat = 41.145556; lon = -73.995 }

let origin = { lat = 0.; lon = 0. }
let origin_offset = { lat = 0.; lon = 0.01 }

let test_arc_length () =
  let d = arc_length palo_alto new_york_city in
  assert (d >= 4000e3 && d <= 4200e3);
  let d =
    arc_length
      palo_alto
      { palo_alto with lon = palo_alto.lon +. 1e-9 }
  in
  assert (d >= 0. && d < 1.);
  let d = arc_length origin origin_offset in
  assert (d >= 0. && d < 1200.);
  true

let tests = [
  "arc length", test_arc_length;
]
