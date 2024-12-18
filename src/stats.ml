type item = { time : Ptime.t; value : float }
type t = item list

let empty = []

let append (x : item) (l : t) = x :: l
