module State = struct
  type state =
    [ `Initialize | `Listening | `Ordering | `Liquidate | `Finished of string ]

  type 'a t = { env : Environment.t; current : state; content : 'a }
  type ('a, 'b) status = Running of 'a t | Shutdown of 'b

  let continue x = Running x
  let shutdown x = Shutdown x

  let init env =
    { env; current = `Initialize; content = Trading_types.Bars.empty }
end
