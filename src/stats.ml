type item = { time : Time.t; value : float } [@@deriving yojson]
type t = item list [@@deriving yojson]

let empty = []
let append (x : item) (l : t) = x :: l
let compare x y = Ptime.compare x.time y.time
let sort (x : t) = List.sort compare x
