type t = Ptime.t

let pp = Ptime.pp

let of_int ms =
  let int = Int64.of_int ms in
  let seconds = Int64.div int 1000L in
  let frac_s = Int64.to_int (Int64.rem int 1000L) in
  match
    Ptime.of_float_s (Int64.to_float seconds +. (float_of_int frac_s /. 1000.0))
  with
  | Some ptime -> ptime
  | None -> failwith "Invalid timestamp"

let pp_int fmt x =
  let x = of_int x in
  pp fmt x

let of_ymd x =
  (* This is meant to be parsing things like YYYY-MM-DD *)
  let rfc = x ^ "T00:00:00Z" in
  match Ptime.of_rfc3339 rfc with
  | Ok (t, _, _) -> t
  | Error _ ->
    invalid_arg
    @@ Format.asprintf "Invalid time in my time module (ymd)? %s" rfc

let to_ymd (x : t) =
  Ptime.to_date_time x |> fun (date, _) ->
  date |> fun (y, m, d) -> Format.asprintf "%d-%02d-%02d" y m d

let of_string x =
  match Ptime.of_rfc3339 x with
  | Ok (t, _, _) -> t
  | Error _ ->
    let tz_added = x ^ "Z" in
    (try Ptime.of_rfc3339 tz_added |> Result.get_exn |> fun (t, _, _) -> t with
    | _ -> invalid_arg @@ Format.asprintf "Invalid time in my time module? %s" x)

let equal = Ptime.equal
let to_string x = Ptime.to_rfc3339 x
let hash x = String.hash @@ to_string x
let t_of_yojson (x : Yojson.Safe.t) = string_of_yojson x |> of_string
let yojson_of_t x = yojson_of_string @@ to_string x

let find_closest (time : t) (l : t list) =
  let times_array = Array.of_list l in
  let a =
    Array.map
      (fun x -> Ptime.diff time x |> Ptime.Span.to_float_s |> Float.abs)
      times_array
  in
  Array.min Float.compare a

let get_todays_date () =
  let time = Unix.time () in
  let now =
    Ptime.of_float_s time |> Option.get_exn_or "time.ml: get_todays_date float"
  in
  Eio.traceln "%s" (to_ymd now);
  let res = to_ymd now |> of_ymd in
  Eio.traceln "%s" (to_ymd res);
  res

let of_float_res x =
  Ptime.of_float_s x |> function
  | None -> Error.fatal "Bad time in Time.of_float_res"
  | Some x -> Result.return x

let subtract_14_days (ptime_value : Ptime.t) =
  let fourteen_days_in_seconds = -1 * (14 * 86400) in
  (* 14 days * 86400 seconds/day *)
  let sub =
    Ptime.add_span ptime_value (Ptime.Span.of_int_s fourteen_days_in_seconds)
  in
  match sub with
  | Some t -> t
  | None -> invalid_arg "Time.subtract_14_days: invalid time"

(* === Timezone and Market Timing Functions === *)

(* Helper: get ISO weekday (1=Monday, 7=Sunday) from date *)
let weekday_of_date year month day =
  let ptime = Ptime.of_date (year, month, day)
              |> Option.get_exn_or "time.ml: weekday_of_date invalid date" in
  let wd = Ptime.weekday ptime in
  match wd with
  | `Mon -> 1 | `Tue -> 2 | `Wed -> 3 | `Thu -> 4
  | `Fri -> 5 | `Sat -> 6 | `Sun -> 7

(* Find the day of month for the nth occurrence of a weekday (1-7, Mon-Sun) *)
let find_nth_weekday year month target_weekday n =
  let first_weekday = weekday_of_date year month 1 in
  (* Days until first occurrence of target weekday *)
  let days_to_first =
    if target_weekday >= first_weekday
    then target_weekday - first_weekday
    else 7 - (first_weekday - target_weekday)
  in
  1 + days_to_first + (n - 1) * 7

(* Check if a date is in Daylight Saving Time (US Eastern Time rules) *)
(* DST starts: 2nd Sunday in March at 2 AM EST → 3 AM EDT *)
(* DST ends: 1st Sunday in November at 2 AM EDT → 1 AM EST *)
let is_dst year month day hour =
  let dst_start_day = find_nth_weekday year 3 7 2 in  (* 2nd Sunday in March *)
  let dst_end_day = find_nth_weekday year 11 7 1 in   (* 1st Sunday in November *)

  match month with
  | m when m < 3 -> false (* January, February *)
  | m when m > 11 -> false (* December *)
  | 3 -> (* March: DST starts on 2nd Sunday at 2 AM *)
      if day > dst_start_day then true
      else if day < dst_start_day then false
      else hour >= 2 (* On the day itself, check hour *)
  | m when m >= 4 && m <= 10 -> true (* April through October *)
  | 11 -> (* November: DST ends on 1st Sunday at 2 AM *)
      if day < dst_end_day then true
      else if day > dst_end_day then false
      else hour < 2 (* On the day itself, check hour *)
  | _ -> false

(* Get UTC offset for Eastern Time (-4 for EDT, -5 for EST) *)
let et_utc_offset_hours (unix_time : float) : float =
  let ptime = Ptime.of_float_s unix_time
              |> Option.get_exn_or "time.ml: et_utc_offset_hours invalid timestamp" in
  let ((year, month, day), ((hour, _, _), _)) = Ptime.to_date_time ptime in
  if is_dst year month day hour then -4.0 (* EDT = UTC-4 *)
  else -5.0 (* EST = UTC-5 *)

(* Convert UTC Unix timestamp to Eastern Time, returning minutes since midnight ET *)
(* Returns a value in [0, 1440) representing time of day *)
let time_of_day_et (unix_time : float) : float =
  let ptime = Ptime.of_float_s unix_time
              |> Option.get_exn_or "time.ml: time_of_day_et invalid timestamp" in
  let (_, ((hour, minute, second), _)) = Ptime.to_date_time ptime in
  let offset_hours = et_utc_offset_hours unix_time in

  (* Apply offset to get ET hour *)
  let et_hour_raw = float_of_int hour +. offset_hours in
  let et_hour =
    if Float.compare et_hour_raw 0.0 < 0 then et_hour_raw +. 24.0
    else if Float.compare et_hour_raw 24.0 >= 0 then et_hour_raw -. 24.0
    else et_hour_raw
  in

  (* Convert to minutes since midnight *)
  et_hour *. 60.0 +. float_of_int minute +. (float_of_int second /. 60.0)

(* Market hours constants (minutes since midnight ET) *)
let market_open_minutes = 9.0 *. 60.0 +. 30.0  (* 9:30 AM = 570 minutes *)
let market_close_minutes = 16.0 *. 60.0         (* 4:00 PM = 960 minutes *)

(* Returns minutes since market open (9:30 AM ET) - can be negative if before open *)
let minutes_since_open (unix_time : float) : float =
  let tod = time_of_day_et unix_time in
  tod -. market_open_minutes

(* Returns minutes until market close (4:00 PM ET) - can be negative if after close *)
let minutes_until_close (unix_time : float) : float =
  let tod = time_of_day_et unix_time in
  market_close_minutes -. tod
