module Log = (val Logs.src_log Logs.(Src.create "time"))

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
      @@ Format.asprintf "Invalid time in my time module (ymd)? %s" x

let of_string x =
  match Ptime.of_rfc3339 x with
  | Ok (t, _, _) -> t
  | Error _ ->
      invalid_arg @@ Format.asprintf "Invalid time in my time module? %s" x
