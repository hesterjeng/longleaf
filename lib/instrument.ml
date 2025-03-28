type t = Security of string | Contract of Contract.t [@@deriving show, eq]

let symbol = function Security x -> x | Contract x -> x.symbol
let yojson_of_t (x : t) : Yojson.Safe.t = `String (symbol x)
let t_of_yojson (_ : Yojson.Safe.t) = invalid_arg "Instrument.t_of_yojson NYI"
let security x = Security x
