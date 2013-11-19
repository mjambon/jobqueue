(* Geographic coordinates in degrees *)
type latlon = {
  lat: float;
  lon: float
}

(* Geocentric coordinates in meters *)
type xyz = {
  x: float;
  y: float;
  z: float;
}

let earth_radius = 6.371e6
let pi = acos (-1.)

let radians deg =
  (pi /. 180.) *. deg

let xyz_of_latlon {lat; lon} =
  let lat = radians lat in
  let lon = radians lon in
  let r = earth_radius in
  {
    x = r *. cos lat *. cos lon;
    y = r *. cos lat *. sin lon;
    z = r *. sin lat;
  }

let dot p1 p2 =
     p1.x *. p2.x
  +. p1.y *. p2.y
  +. p1.z *. p2.z

let div { x; y; z } r = {
  x = x /. r;
  y = y /. r;
  z = z /. r;
}

let arc_length_xyz p1 p2 =
  let angle = acos (dot p1 p2 /. (earth_radius ** 2.)) in
  earth_radius *. angle

let arc_length_xyz p1 p2 =
  if p1 = p2 then 0.
  else
    let angle = acos (dot p1 p2 /. (earth_radius *. earth_radius)) in
    earth_radius *. angle

let arc_length loc1 loc2 =
  arc_length_xyz (xyz_of_latlon loc1) (xyz_of_latlon loc2)

let arc_length_opt opt_loc1 opt_loc2 =
  match opt_loc1, opt_loc2 with
  | None, _
  | _, None -> 0.
  | Some loc1, Some loc2 -> arc_length loc1 loc2

let palo_alto = { lat = 37.429167; lon = -122.138056 }
let san_francisco = { lat = 37.7833; lon = -122.4167 }
let new_york_city = { lat = 41.145556; lon = -73.995 }

let test_arc_length () =
  let d = arc_length palo_alto new_york_city in
  assert (d >= 4000e3 && d <= 4200e3);
  let d =
    arc_length
      palo_alto
      { palo_alto with lon = palo_alto.lon +. 1e-9 }
  in
  assert (d >= 0. && d < 1.);
  true

let tests = [
  "arc length", test_arc_length;
]
