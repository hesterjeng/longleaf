type t = Item.t Vector.vector

let pp : t Vector.printer = Vector.pp Item.pp
let empty () = Vector.create ()

module V2 = struct
  open Bigarray

  type t = {
    data : (float, float64_elt, c_layout) Array2.t;
    current : int;
    indicators_computed : bool;
  }

  type slice = (float, float64_elt, c_layout) Array1.t

  module Row = struct
    type t = Index | Time | Last | Open | High | Low | Close | Volume | SMA

    let to_int = function
      | Index -> 0
      | Time -> 1
      | Last -> 2
      | Open -> 3
      | High -> 4
      | Low -> 5
      | Close -> 6
      | Volume -> 7
      | SMA -> 8
  end

  (* Row 0 : i (index)*)
  (* Row 1 : time (float) *)
  (* Row 2 : price *)
  (* Row 3 : open *)
  (* Row 4 : high *)
  (* Row 5 : low *)
  (* Row 6 : close *)
  (* Row 7:  volume *)
  (* Row 8 : SMA *)

  let set (res : t) (x : Row.t) i value =
    Array2.set res.data (Row.to_int x) i @@ value

  let get (res : t) (x : Row.t) i = Array2.get res.data (Row.to_int x) i

  let make size : t =
    {
      data = Array2.create float64 c_layout 9 size;
      current = 0;
      indicators_computed = false;
    }

  let t_of_yojson (json : Yojson.Safe.t) =
    match json with
    | `List items ->
      let size = List.length items in
      let res = make size in
      let rec aux i l =
        match l with
        | [] -> ()
        | current_j :: xs ->
          let current = Item.t_of_yojson current_j in
          assert (i < size);
          assert (get res SMA i =. 0.0);
          set res Index i (Float.of_int i);
          set res Time i @@ Ptime.to_float_s @@ Item.timestamp current;
          set res Last i @@ Item.last current;
          set res Open i @@ Item.open_ current;
          set res High i @@ Item.high current;
          set res Low i @@ Item.low current;
          set res Close i @@ Item.close current;
          set res Volume i @@ Float.of_int @@ Item.volume current;
          aux (i + 1) xs
      in
      aux 0 items;
      Result.return res
    | _ ->
      Error.json "Expected a list of datapoints in Price_history.V2.t_of_yojson"
end
