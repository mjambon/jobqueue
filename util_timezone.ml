type t = string

let fallback_timezone : t = "America/Los_Angeles"

(*
   OCaml does not provide Unix.clearenv, but we need to set TZ back
   to our local timezone after being done with [with_timezone].
   We achieve that by setting TZ if it is not set initially.
*)
let init_timezone () =
  try ignore (Unix.getenv "TZ")
  with Not_found -> Unix.putenv "TZ" fallback_timezone

let () = init_timezone ()

let with_timezone tz f =
  let old_tz = try Unix.getenv "TZ" with Not_found -> fallback_timezone in
  let fin () = Unix.putenv "TZ" old_tz in
  Unix.putenv "TZ" tz;
  try
    let res = f () in
    fin ();
    res
  with e ->
    fin ();
    Trax.raise __LOC__ e

let utc_of_local tz local =
  with_timezone tz (fun () ->
    let local_tm, subsecond = Util_localtime.to_unix_tm local in
    let unixtime, utc_tm = Unix.mktime local_tm in
    Util_time.of_float (unixtime +. subsecond)
  )

let local_of_utc tz utc =
  with_timezone tz (fun () ->
    let unixtime = Util_time.to_float utc in
    let x = Unix.localtime unixtime in
    Util_localtime.of_unix_tm x (Util_localtime.fpart unixtime)
  )

let midnight = Util_timeonly.create ~hour:0 ~min:0 ~sec:0.

let utc_of_day tz day =
  utc_of_local tz (Util_localtime.of_pair day midnight)

(*
  TODO: list timezones for each country and countries for each continent.
*)
let timezones =
  [
    (* most common US timezones from West to East *)
    "US/Pacific"; "US/Mountain"; "US/Central"; "US/Eastern";

    (* all timezones, alphabetical *)
    "Africa/Abidjan"; "Africa/Accra"; "Africa/Addis_Ababa"; "Africa/Algiers";
    "Africa/Asmara"; "Africa/Asmera"; "Africa/Bamako"; "Africa/Bangui";
    "Africa/Banjul"; "Africa/Bissau"; "Africa/Blantyre";
    "Africa/Brazzaville"; "Africa/Bujumbura"; "Africa/Cairo";
    "Africa/Casablanca"; "Africa/Ceuta"; "Africa/Conakry"; "Africa/Dakar";
    "Africa/Dar_es_Salaam"; "Africa/Djibouti"; "Africa/Douala";
    "Africa/El_Aaiun"; "Africa/Freetown"; "Africa/Gaborone"; "Africa/Harare";
    "Africa/Johannesburg"; "Africa/Kampala"; "Africa/Khartoum";
    "Africa/Kigali"; "Africa/Kinshasa"; "Africa/Lagos"; "Africa/Libreville";
    "Africa/Lome"; "Africa/Luanda"; "Africa/Lubumbashi"; "Africa/Lusaka";
    "Africa/Malabo"; "Africa/Maputo"; "Africa/Maseru"; "Africa/Mbabane";
    "Africa/Mogadishu"; "Africa/Monrovia"; "Africa/Nairobi";
    "Africa/Ndjamena"; "Africa/Niamey"; "Africa/Nouakchott";
    "Africa/Ouagadougou"; "Africa/Porto-Novo"; "Africa/Sao_Tome";
    "Africa/Timbuktu"; "Africa/Tripoli"; "Africa/Tunis"; "Africa/Windhoek";
    "America/Adak"; "America/Anchorage"; "America/Anguilla";
    "America/Antigua"; "America/Araguaina"; "America/Argentina/Buenos_Aires";
    "America/Argentina/Catamarca"; "America/Argentina/ComodRivadavia";
    "America/Argentina/Cordoba"; "America/Argentina/Jujuy";
    "America/Argentina/La_Rioja"; "America/Argentina/Mendoza";
    "America/Argentina/Rio_Gallegos"; "America/Argentina/Salta";
    "America/Argentina/San_Juan"; "America/Argentina/San_Luis";
    "America/Argentina/Tucuman"; "America/Argentina/Ushuaia";
    "America/Aruba"; "America/Asuncion"; "America/Atikokan"; "America/Atka";
    "America/Bahia"; "America/Barbados"; "America/Belem"; "America/Belize";
    "America/Blanc-Sablon"; "America/Boa_Vista"; "America/Bogota";
    "America/Boise"; "America/Buenos_Aires"; "America/Cambridge_Bay";
    "America/Campo_Grande"; "America/Cancun"; "America/Caracas";
    "America/Catamarca"; "America/Cayenne"; "America/Cayman";
    "America/Chicago"; "America/Chihuahua"; "America/Coral_Harbour";
    "America/Cordoba"; "America/Costa_Rica"; "America/Cuiaba";
    "America/Curacao"; "America/Danmarkshavn"; "America/Dawson";
    "America/Dawson_Creek"; "America/Denver"; "America/Detroit";
    "America/Dominica"; "America/Edmonton"; "America/Eirunepe";
    "America/El_Salvador"; "America/Ensenada"; "America/Fort_Wayne";
    "America/Fortaleza"; "America/Glace_Bay"; "America/Godthab";
    "America/Goose_Bay"; "America/Grand_Turk"; "America/Grenada";
    "America/Guadeloupe"; "America/Guatemala"; "America/Guayaquil";
    "America/Guyana"; "America/Halifax"; "America/Havana";
    "America/Hermosillo"; "America/Indiana/Indianapolis";
    "America/Indiana/Knox"; "America/Indiana/Marengo";
    "America/Indiana/Petersburg"; "America/Indiana/Tell_City";
    "America/Indiana/Vevay"; "America/Indiana/Vincennes";
    "America/Indiana/Winamac"; "America/Indianapolis"; "America/Inuvik";
    "America/Iqaluit"; "America/Jamaica"; "America/Jujuy"; "America/Juneau";
    "America/Kentucky/Louisville"; "America/Kentucky/Monticello";
    "America/Knox_IN"; "America/La_Paz"; "America/Lima";
    "America/Los_Angeles"; "America/Louisville"; "America/Maceio";
    "America/Managua"; "America/Manaus"; "America/Marigot";
    "America/Martinique"; "America/Matamoros"; "America/Mazatlan";
    "America/Mendoza"; "America/Menominee"; "America/Merida";
    "America/Mexico_City"; "America/Miquelon"; "America/Moncton";
    "America/Monterrey"; "America/Montevideo"; "America/Montreal";
    "America/Montserrat"; "America/Nassau"; "America/New_York";
    "America/Nipigon"; "America/Nome"; "America/Noronha";
    "America/North_Dakota/Center"; "America/North_Dakota/New_Salem";
    "America/Ojinaga"; "America/Panama"; "America/Pangnirtung";
    "America/Paramaribo"; "America/Phoenix"; "America/Port-au-Prince";
    "America/Port_of_Spain"; "America/Porto_Acre"; "America/Porto_Velho";
    "America/Puerto_Rico"; "America/Rainy_River"; "America/Rankin_Inlet";
    "America/Recife"; "America/Regina"; "America/Resolute";
    "America/Rio_Branco"; "America/Rosario"; "America/Santa_Isabel";
    "America/Santarem"; "America/Santiago"; "America/Santo_Domingo";
    "America/Sao_Paulo"; "America/Scoresbysund"; "America/Shiprock";
    "America/St_Barthelemy"; "America/St_Johns"; "America/St_Kitts";
    "America/St_Lucia"; "America/St_Thomas"; "America/St_Vincent";
    "America/Swift_Current"; "America/Tegucigalpa"; "America/Thule";
    "America/Thunder_Bay"; "America/Tijuana"; "America/Toronto";
    "America/Tortola"; "America/Vancouver"; "America/Virgin";
    "America/Whitehorse"; "America/Winnipeg"; "America/Yakutat";
    "America/Yellowknife"; "Antarctica/Casey"; "Antarctica/Davis";
    "Antarctica/DumontDUrville"; "Antarctica/Macquarie"; "Antarctica/Mawson";
    "Antarctica/McMurdo"; "Antarctica/Palmer"; "Antarctica/Rothera";
    "Antarctica/South_Pole"; "Antarctica/Syowa"; "Antarctica/Vostok";
    "Arctic/Longyearbyen"; "Asia/Aden"; "Asia/Almaty"; "Asia/Amman";
    "Asia/Anadyr"; "Asia/Aqtau"; "Asia/Aqtobe"; "Asia/Ashgabat";
    "Asia/Ashkhabad"; "Asia/Baghdad"; "Asia/Bahrain"; "Asia/Baku";
    "Asia/Bangkok"; "Asia/Beirut"; "Asia/Bishkek"; "Asia/Brunei";
    "Asia/Calcutta"; "Asia/Choibalsan"; "Asia/Chongqing"; "Asia/Chungking";
    "Asia/Colombo"; "Asia/Dacca"; "Asia/Damascus"; "Asia/Dhaka"; "Asia/Dili";
    "Asia/Dubai"; "Asia/Dushanbe"; "Asia/Gaza"; "Asia/Harbin";
    "Asia/Ho_Chi_Minh"; "Asia/Hong_Kong"; "Asia/Hovd"; "Asia/Irkutsk";
    "Asia/Istanbul"; "Asia/Jakarta"; "Asia/Jayapura"; "Asia/Jerusalem";
    "Asia/Kabul"; "Asia/Kamchatka"; "Asia/Karachi"; "Asia/Kashgar";
    "Asia/Kathmandu"; "Asia/Katmandu"; "Asia/Kolkata"; "Asia/Krasnoyarsk";
    "Asia/Kuala_Lumpur"; "Asia/Kuching"; "Asia/Kuwait"; "Asia/Macao";
    "Asia/Macau"; "Asia/Magadan"; "Asia/Makassar"; "Asia/Manila";
    "Asia/Muscat"; "Asia/Nicosia"; "Asia/Novokuznetsk"; "Asia/Novosibirsk";
    "Asia/Omsk"; "Asia/Oral"; "Asia/Phnom_Penh"; "Asia/Pontianak";
    "Asia/Pyongyang"; "Asia/Qatar"; "Asia/Qyzylorda"; "Asia/Rangoon";
    "Asia/Riyadh"; "Asia/Riyadh87"; "Asia/Riyadh88"; "Asia/Riyadh89";
    "Asia/Saigon"; "Asia/Sakhalin"; "Asia/Samarkand"; "Asia/Seoul";
    "Asia/Shanghai"; "Asia/Singapore"; "Asia/Taipei"; "Asia/Tashkent";
    "Asia/Tbilisi"; "Asia/Tehran"; "Asia/Tel_Aviv"; "Asia/Thimbu";
    "Asia/Thimphu"; "Asia/Tokyo"; "Asia/Ujung_Pandang"; "Asia/Ulaanbaatar";
    "Asia/Ulan_Bator"; "Asia/Urumqi"; "Asia/Vientiane"; "Asia/Vladivostok";
    "Asia/Yakutsk"; "Asia/Yekaterinburg"; "Asia/Yerevan"; "Atlantic/Azores";
    "Atlantic/Bermuda"; "Atlantic/Canary"; "Atlantic/Cape_Verde";
    "Atlantic/Faeroe"; "Atlantic/Faroe"; "Atlantic/Jan_Mayen";
    "Atlantic/Madeira"; "Atlantic/Reykjavik"; "Atlantic/South_Georgia";
    "Atlantic/St_Helena"; "Atlantic/Stanley"; "Australia/ACT";
    "Australia/Adelaide"; "Australia/Brisbane"; "Australia/Broken_Hill";
    "Australia/Canberra"; "Australia/Currie"; "Australia/Darwin";
    "Australia/Eucla"; "Australia/Hobart"; "Australia/LHI";
    "Australia/Lindeman"; "Australia/Lord_Howe"; "Australia/Melbourne";
    "Australia/NSW"; "Australia/North"; "Australia/Perth";
    "Australia/Queensland"; "Australia/South"; "Australia/Sydney";
    "Australia/Tasmania"; "Australia/Victoria"; "Australia/West";
    "Australia/Yancowinna"; "Brazil/Acre"; "Brazil/DeNoronha"; "Brazil/East";
    "Brazil/West"; "CET"; "CST6CDT"; "Canada/Atlantic"; "Canada/Central";
    "Canada/East-Saskatchewan"; "Canada/Eastern"; "Canada/Mountain";
    "Canada/Newfoundland"; "Canada/Pacific"; "Canada/Saskatchewan";
    "Canada/Yukon"; "Chile/Continental"; "Chile/EasterIsland"; "Cuba"; "EET";
    "EST"; "EST5EDT"; "Egypt"; "Eire"; "Etc/GMT"; "Etc/GMT+0"; "Etc/GMT+1";
    "Etc/GMT+10"; "Etc/GMT+11"; "Etc/GMT+12"; "Etc/GMT+2"; "Etc/GMT+3";
    "Etc/GMT+4"; "Etc/GMT+5"; "Etc/GMT+6"; "Etc/GMT+7"; "Etc/GMT+8";
    "Etc/GMT+9"; "Etc/GMT-0"; "Etc/GMT-1"; "Etc/GMT-10"; "Etc/GMT-11";
    "Etc/GMT-12"; "Etc/GMT-13"; "Etc/GMT-14"; "Etc/GMT-2"; "Etc/GMT-3";
    "Etc/GMT-4"; "Etc/GMT-5"; "Etc/GMT-6"; "Etc/GMT-7"; "Etc/GMT-8";
    "Etc/GMT-9"; "Etc/GMT0"; "Etc/Greenwich"; "Etc/UCT"; "Etc/UTC";
    "Etc/Universal"; "Etc/Zulu"; "Europe/Amsterdam"; "Europe/Andorra";
    "Europe/Athens"; "Europe/Belfast"; "Europe/Belgrade"; "Europe/Berlin";
    "Europe/Bratislava"; "Europe/Brussels"; "Europe/Bucharest";
    "Europe/Budapest"; "Europe/Chisinau"; "Europe/Copenhagen";
    "Europe/Dublin"; "Europe/Gibraltar"; "Europe/Guernsey";
    "Europe/Helsinki"; "Europe/Isle_of_Man"; "Europe/Istanbul";
    "Europe/Jersey"; "Europe/Kaliningrad"; "Europe/Kiev"; "Europe/Lisbon";
    "Europe/Ljubljana"; "Europe/London"; "Europe/Luxembourg";
    "Europe/Madrid"; "Europe/Malta"; "Europe/Mariehamn"; "Europe/Minsk";
    "Europe/Monaco"; "Europe/Moscow"; "Europe/Nicosia"; "Europe/Oslo";
    "Europe/Paris"; "Europe/Podgorica"; "Europe/Prague"; "Europe/Riga";
    "Europe/Rome"; "Europe/Samara"; "Europe/San_Marino"; "Europe/Sarajevo";
    "Europe/Simferopol"; "Europe/Skopje"; "Europe/Sofia"; "Europe/Stockholm";
    "Europe/Tallinn"; "Europe/Tirane"; "Europe/Tiraspol"; "Europe/Uzhgorod";
    "Europe/Vaduz"; "Europe/Vatican"; "Europe/Vienna"; "Europe/Vilnius";
    "Europe/Volgograd"; "Europe/Warsaw"; "Europe/Zagreb";
    "Europe/Zaporozhye"; "Europe/Zurich"; "GB"; "GB-Eire"; "GMT"; "GMT+0";
    "GMT-0"; "GMT0"; "Greenwich"; "HST"; "Hongkong"; "Iceland";
    "Indian/Antananarivo"; "Indian/Chagos"; "Indian/Christmas";
    "Indian/Cocos"; "Indian/Comoro"; "Indian/Kerguelen"; "Indian/Mahe";
    "Indian/Maldives"; "Indian/Mauritius"; "Indian/Mayotte";
    "Indian/Reunion"; "Iran"; "Israel"; "Jamaica"; "Japan"; "Kwajalein";
    "Libya"; "MET"; "MST"; "MST7MDT"; "Mexico/BajaNorte"; "Mexico/BajaSur";
    "Mexico/General"; "Mideast/Riyadh87"; "Mideast/Riyadh88";
    "Mideast/Riyadh89"; "NZ"; "NZ-CHAT"; "Navajo"; "PRC"; "PST8PDT";
    "Pacific/Apia"; "Pacific/Auckland"; "Pacific/Chatham"; "Pacific/Easter";
    "Pacific/Efate"; "Pacific/Enderbury"; "Pacific/Fakaofo"; "Pacific/Fiji";
    "Pacific/Funafuti"; "Pacific/Galapagos"; "Pacific/Gambier";
    "Pacific/Guadalcanal"; "Pacific/Guam"; "Pacific/Honolulu";
    "Pacific/Johnston"; "Pacific/Kiritimati"; "Pacific/Kosrae";
    "Pacific/Kwajalein"; "Pacific/Majuro"; "Pacific/Marquesas";
    "Pacific/Midway"; "Pacific/Nauru"; "Pacific/Niue"; "Pacific/Norfolk";
    "Pacific/Noumea"; "Pacific/Pago_Pago"; "Pacific/Palau";
    "Pacific/Pitcairn"; "Pacific/Ponape"; "Pacific/Port_Moresby";
    "Pacific/Rarotonga"; "Pacific/Saipan"; "Pacific/Samoa"; "Pacific/Tahiti";
    "Pacific/Tarawa"; "Pacific/Tongatapu"; "Pacific/Truk"; "Pacific/Wake";
    "Pacific/Wallis"; "Pacific/Yap"; "Poland"; "Portugal"; "ROC"; "ROK";
    "Singapore"; "Turkey"; "UCT"; "US/Alaska"; "US/Aleutian"; "US/Arizona";
    "US/Central"; "US/East-Indiana"; "US/Eastern"; "US/Hawaii";
    "US/Indiana-Starke"; "US/Michigan"; "US/Mountain"; "US/Pacific";
    "US/Pacific-New"; "US/Samoa"; "UTC"; "Universal"; "W-SU"; "WET"; "Zulu"
  ]

let is_supported =
  let tbl = Hashtbl.create 200 in
  List.iter (fun s -> Hashtbl.replace tbl s ()) timezones;
  fun tz -> Hashtbl.mem tbl tz

let timezone_of_string s =
  if not (is_supported s) then
    invalid_arg "Util_timezone.timezone_of_string";
  s

let timezone_mapping = [
  ("America/Chicago", "Central Time");
  ("America/Denver", "Mountain Time");
  ("America/Los_Angeles", "Pacific Time");
  ("America/New_York", "Eastern Time");
]

(* Convert standard names into colloquial ones *)
let display_name tz =
  try List.assoc tz timezone_mapping
  with Not_found -> "(Time zone: " ^ tz ^ ")"

let test_utc_of_local () =
  let tz = "America/New_York" in
  let local = Util_localtime.of_string "2016-02-13T00:39:57" in
  let utc = utc_of_local tz local in
  let out = Util_time.to_float utc in
  let expected =
    Util_time.(to_float (of_string "2016-02-12T21:39:57.000-08:00")) in
  out = expected

let test_local_of_utc () =
  let tz = "America/New_York" in
  let utc = Util_time.of_string "2016-02-13T00:39:57Z" in
  let local = local_of_utc tz utc in
  let out = Util_localtime.to_string local in
  let expected = "2016-02-12T19:39:57.000" in
  out = expected

let tests = [
  "local -> utc", test_utc_of_local;
  "utc -> local", test_local_of_utc;
]
