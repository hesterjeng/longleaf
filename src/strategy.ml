module Log = (val Logs.src_log Logs.(Src.create "strategy"))

module Account = struct
  type t = { money_available : float; shares_owned : int } [@@deriving show]

  let buy x price =
    if x.money_available <. price then None
    else (
      Log.app (fun k -> k "Buy!");
      Option.some
      @@ {
           money_available = x.money_available -. price;
           shares_owned = x.shares_owned + 1;
         })

  let sell x price =
    if x.shares_owned > 0 then (
      Log.app (fun k -> k "Sell!");
      Option.some
      @@ {
           money_available = x.money_available +. price;
           shares_owned = x.shares_owned - 1;
         })
    else None

  let value price x = x.money_available +. (Float.of_int x.shares_owned *. price)

  let step average_closing_price current_price (x : t) =
    let res =
      if current_price <. average_closing_price then buy x current_price
      else sell x current_price
    in
    match res with Some res -> res | None -> x
end

let initial_account : Account.t = { money_available = 1000.0; shares_owned = 0 }

let run average_closing_price price_data account =
  Array.fold_left
    (fun current_account current_price ->
      Account.step average_closing_price current_price current_account)
    account price_data

let basic (x : Data.t) =
  let closing_prices = Owl.Arr.to_array x.close_p in
  let opening_prices = Owl.Arr.to_array x.open_p in
  let average_closing_price = Owl.Stats.mean closing_prices in
  let final_price = opening_prices.(Array.length opening_prices - 1) in
  let result = run average_closing_price opening_prices initial_account in
  Log.app (fun k ->
      k "@[%a@]@.@[%f@]@." Account.pp result (Account.value final_price result));
  ()

module type Data_input = sig
  val data : float array
end

module Make (Training : Data_input) (Testing : Data_input) = struct
  let smoke () = Training.data
end
