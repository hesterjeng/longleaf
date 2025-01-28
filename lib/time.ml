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
      invalid_arg @@ Format.asprintf "Invalid time in my time module? %s" x

let to_string x = Ptime.to_rfc3339 x
let t_of_yojson (x : Yojson.Safe.t) = string_of_yojson x |> of_string
let yojson_of_t x = yojson_of_string @@ to_string x

let find_closest (time : t) (l : t list) =
  let times_array = Array.of_list l in
  let minimum_difference =
    List.map
      (fun x -> Ptime.diff time x |> Ptime.Span.to_float_s |> Float.abs)
      l
    |> Array.of_list |> Owl.Stats.min_i
  in
  Array.get_safe times_array minimum_difference |> function
  | Some res -> res
  | None -> invalid_arg "Expected to find a closest time in Time.find_closest"

let get_todays_date () =
  let time = Unix.time () in
  let now =
    Ptime.of_float_s time |> Option.get_exn_or "time.ml: get_todays_date float"
  in
  Eio.traceln "%s" (to_ymd now);
  let res = to_ymd now |> of_ymd in
  Eio.traceln "%s" (to_ymd res);
  res

let subtract_30_days (ptime_value : Ptime.t) =
  let thirty_days_in_seconds = -1 * (30 * 86400) in
  (* 30 days * 86400 seconds/day *)
  let sub =
    Ptime.add_span ptime_value (Ptime.Span.of_int_s thirty_days_in_seconds)
  in
  match sub with
  | Some t -> t
  | None -> invalid_arg "Time.subtract_30_days: invalid time"
