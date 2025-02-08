type t = { inactive : Order.t list; active : Order.t list }

let all h = h.inactive @ h.active
let sort h = List.sort Order.cmp_timestamp h
let inactive h = h.inactive
let active h = h.active

let yojson_of_t (h : t) : Yojson.Safe.t =
  let l = all h in
  `List (List.map Order.yojson_of_t l)

let add x order = { x with active = order :: x.active }
let empty = { inactive = []; active = [] }
let length h = List.length h.inactive + List.length h.active

let complete history order =
  {
    inactive = order :: history.inactive;
    active = List.filter (fun o -> not @@ Order.equal o order) history.active;
  }
