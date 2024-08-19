include Ptime

let convert_timestamp_to_date ts =
  match Ptime.of_float_s (Int64.to_float ts) with
  | Some date_time ->
      let (date, time) = Ptime.to_date_time date_time in
      let (year, month, day) = date in
      let ((hours, minutes, seconds), _) = time in
      Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
        year month day hours minutes seconds
  | None -> "Invalid timestamp"
