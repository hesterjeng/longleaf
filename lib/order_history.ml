type t = { inactive : Order.t list; active : Order.t list }

let all h = h.inactive @ h.active |> List.sort Order.cmp_timestamp

let yojson_of_t (h : t) : Yojson.Safe.t =
  let l = all h in
  `List (List.map Order.yojson_of_t l)

let activate x order = { x with active = order :: x.active }

let complete history order =
  {
    inactive = order :: history.inactive;
    active = List.filter (fun o -> not @@ Order.equal o order) history.active;
  }
