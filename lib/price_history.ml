type t = Item.t Vector.vector

let pp : t Vector.printer = Vector.pp Item.pp
let empty () = Vector.create ()

module V2 = struct
  open Bigarray

  type t = { data : (float, float64_elt, c_layout) Array2.t; current : int }
  type slice = (float, float64_elt, c_layout) Array1.t

  (* Row 0 : time (float) *)
  (* Row 1 : price *)
  (* Row 2 : open *)
  (* Row 3 : high *)
  (* Row 4 : low *)
  (* Row 5 : close *)
  (* Row 6:  volume *)
  (* Row 7 : SMA *)

  let make size : t =
    { data = Array2.create float64 c_layout 8 size; current = 0 }

  let t_of_yojson (json : Yojson.Safe.t) =
    match json with
    | `List items ->
      let arr = Array.of_list items |> Array.map Item.t_of_yojson in
      let size = Array.length arr in
      let res = make size in
      for i = 0 to size - 1 do
        let crt = Array.get arr i in
        Array2.set res.data 0 i @@ Ptime.to_float_s @@ Item.timestamp crt;
        Array2.set res.data 1 i @@ Item.last crt;
        Array2.set res.data 2 i @@ Item.open_ crt;
        Array2.set res.data 3 i @@ Item.high crt;
        Array2.set res.data 4 i @@ Item.low crt;
        Array2.set res.data 5 i @@ Item.close crt;
        Array2.set res.data 6 i @@ Float.of_int @@ Item.volume crt;
        assert (Array2.get res.data 7 i =. 0.0);
        ()
      done;
      Result.return res
    | _ ->
      Error.json "Expected a list of datapoints in Price_history.V2.t_of_yojson"
end
