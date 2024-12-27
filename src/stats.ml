type item = { time : Ptime.t; value : float }
type t = item list

let empty = []
let append (x : item) (l : t) = x :: l
let compare x y = Ptime.compare x.time y.time
let sort (x : t) = List.sort compare x
