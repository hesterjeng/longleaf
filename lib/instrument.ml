type t = Security of string | Contract of Contract.t [@@deriving show, eq]
