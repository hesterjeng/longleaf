type t = { num_orders : int }

let num_orders arr =
  let lengths = Vector.map List.length arr in
  Vector.fold ( + ) 0 lengths

let make orders (_x : Bars.t) =
  let num_orders = num_orders orders in
  { num_orders }
