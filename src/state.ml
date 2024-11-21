type nonlogical_state =
  [ `Initialize | `Listening | `Liquidate | `Finished of string ]
[@@deriving show { with_path = false }]

type logical_state = [ `Ordering ] [@@deriving show { with_path = false }]

type state = [ nonlogical_state | logical_state ]
[@@deriving show { with_path = false }]

type 'a t = {
  current : state;
  bars : Bars.t;
  latest_bars : Bars.t;
  content : 'a;
}

let init () =
  {
    current = `Initialize;
    bars = Bars.empty;
    latest_bars = Bars.empty;
    content = ();
  }
