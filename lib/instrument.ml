type t = Security of string | Contract of Contract.t [@@deriving show]

let yojson_of_t _ : Yojson.Safe.t = `Null
let t_of_yojson _ = invalid_arg "Should not create Instrument.t of yojson."
