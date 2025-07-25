type t = {
  timestamp : Time.t; [@key "t"]
  open_ : float; [@key "o"]
  high : float; [@key "h"]
  low : float; [@key "l"]
  close : float; [@key "c"]
  last : float; [@yojson.default Float.nan]
  volume : int; [@key "v"] (* order : Order.t option; [@default None] *)
}
[@@deriving show { with_path = false }, yojson, make]
[@@yojson.allow_extra_fields]

let t_of_yojson x =
  let p1 = t_of_yojson x in
  if Float.equal p1.last Float.max_finite_value then { p1 with last = p1.close }
  else p1

(* let t_of_yojson x = *)
(*   try *)
(*     t_of_yojson x |> fun (x : t) -> *)
(*     if Float.equal x.last Float.max_finite_value then *)
(*       { x with last = x.close } *)
(*     else x *)
(*   with Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (e, j) -> *)
(*     let exc = Printexc.to_string e in *)
(*     Eio.traceln "@[bar_item:@]@.@[%s@]@.@[%s@]@." exc *)
(*       (Yojson.Safe.to_string j); *)
(*     exit 1 *)

let open_ x = x.open_
(* let order x = x.order *)

(* let add_order (order : Order.t) (x : t) = *)
(*   match x.order with *)
(*   | None -> Result.return @@ { x with order = Some order } *)
(*   | Some prev_order -> *)
(*     let msg = "[error: item.ml] Trying to replace order..." in *)
(*     Eio.traceln "[error: item.ml]: trying to replace@. @[%a@]@. with@. @[%a@]" *)
(*       Order.pp prev_order Order.pp order; *)
(*     Result.fail @@ `FatalError msg *)

let timestamp (x : t) = x.timestamp
let close x = x.close
let high x = x.high
let low x = x.low
let last (x : t) = x.last
let volume x = x.volume

(* let indicators x = x.indicators *)
let compare x y = Ptime.compare x.timestamp y.timestamp
let eq_timestamp x y = compare x y = 0
