module US = struct
  open Printf
  open Unix

  let months =
    [| "January"; "February"; "March"; "April"; "May"; "June";
       "July"; "August"; "September"; "October"; "November"; "December" |]

  let week_days =
    [| "Sunday"; "Monday"; "Tuesday"; "Wednesday";
       "Thursday"; "Friday"; "Saturday" |]

  (* "Wednesday" *)
  let week_day x =
    week_days.(x.tm_wday)

  (* "3" *)
  let month_day x =
    sprintf "%i" x.tm_mday

  (* "03" *)
  let num_month_day x =
    sprintf "%02i" x.tm_mday

  (* "April" *)
  let month x =
    months.(x.tm_mon)

  (* "04" *)
  let num_month x =
    sprintf "%02d" (1 + x.tm_mon)

  (* "1993" *)
  let year x =
    sprintf "%d" (1900 + x.tm_year)

  (* "0699" *)
  let num_year x =
    sprintf "%02d" (1900 + x.tm_year)

  (* "August 13, 2019" *)
  let date_only x =
    sprintf "%s %i, %s"
      (month x) x.tm_mday (year x)

  (* "12/31/2015" *)
  let num_date_only x =
    sprintf "%s/%s/%s"
      (num_month x) (num_month_day x) (num_year x)

  (* "1:30 pm" *)
  let time_only x =
    let h0 = x.tm_hour in
    let m = x.tm_min in
    let h, ampm =
      if h0 < 12 then
        h0, "am"
      else if h0 < 13 then
        12, "pm"
      else
        h0 - 12, "pm"
    in
    sprintf "%d:%02d %s" h m ampm

end
