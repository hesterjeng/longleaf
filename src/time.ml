type t = { date : Ptime.date; time : Ptime.time }

let pp fmt x =
  let ptime = Ptime.of_date_time (x.date, x.time) in
  match ptime with
  | Some time -> Format.fprintf fmt "%a" Ptime.pp time
  | None -> invalid_arg "Illegal datetime!"

let of_timestamp ts =
  match Ptime.of_float_s ts with
  | Some date_time ->
      let date, time = Ptime.to_date_time date_time in
      { date; time }
  | None -> invalid_arg "invalid date/time"

let print_time_array x =
  Owl_dense_ndarray.D.iter
    (fun x -> of_timestamp x |> fun x -> Format.printf "@[%a@]@." pp x)
    x
