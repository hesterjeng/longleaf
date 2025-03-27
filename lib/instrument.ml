type t = Security of string | Contract of Contract.t [@@deriving show, eq]

let symbol = function Security x -> x | Contract x -> x.symbol
let yojson_of_t (x : t) = `String (symbol x)
let security x = Security x
