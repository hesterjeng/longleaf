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
