open Bigarray

type t = {
  data : (float, float64_elt, c_layout) Array2.t;
  current : int;
  size : int;
  indicators_computed : bool;
}

type slice = (float, float64_elt, c_layout) Array1.t

module Row = struct
  type t =
    | Index
    | Time
    | Last
    | Open
    | High
    | Low
    | Close
    | Volume
    | SMA
    | FSO_K
    | FSO_D
    | RSI
  [@@deriving variants]

  let count = List.length Variants.descriptions

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
    | FSO_K -> 9
    | FSO_D -> 10
    | RSI -> 11
end

let length x = Array2.dim2 x.data
let current x = x.current

(* Row 0 : i (index)*)
(* Row 1 : time (float) *)
(* Row 2 : price *)
(* Row 3 : open *)
(* Row 4 : high *)
(* Row 5 : low *)
(* Row 6 : close *)
(* Row 7:  volume *)
(* Row 8 : SMA *)
(* Row 9 : FSO_K *)
(* Row 10 : FSO_D *)
(* Row 11 : RSI *)

let set (res : t) (x : Row.t) i value =
  Array2.set res.data (Row.to_int x) i @@ value

let get (res : t) (x : Row.t) i = Array2.get res.data (Row.to_int x) i
let get_top (res : t) (x : Row.t) = get res x @@ res.current

let make size : t =
  {
    data = Array2.create float64 c_layout Row.count size;
    current = 0;
    size;
    indicators_computed = false;
  }

let copy (x : t) =
  let r = make x.size in
  Array2.blit x.data r.data;
  { r with current = x.current; indicators_computed = x.indicators_computed }

let add_item (x : t) (item : Item.t) =
  let i = x.current in
  try
    set x Index i (Float.of_int i);
    set x Time i @@ Ptime.to_float_s @@ Item.timestamp item;
    set x Last i @@ Item.last item;
    set x Open i @@ Item.open_ item;
    set x High i @@ Item.high item;
    set x Low i @@ Item.low item;
    set x Close i @@ Item.close item;
    set x Volume i @@ Float.of_int @@ Item.volume item;
    Ok ()
  with
  | _ -> Error.fatal "Illegal index accessin Price_history.V2.add_item"

let t_of_yojson (json : Yojson.Safe.t) : (t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  match json with
  | `List items ->
    let size = List.length items in
    let res = make size in
    let rec aux i l acc =
      match l with
      | [] -> acc
      | current_j :: xs ->
        let* acc = acc in
        let current = Item.t_of_yojson current_j in
        assert (i < size);
        assert (get res SMA i =. 0.0);
        let* () = add_item res current in
        aux (i + 1) xs @@ Result.return { acc with current = acc.current + 1 }
    in
    let* res = aux 0 items (Ok res) in
    Result.return res
  | _ ->
    Error.json "Expected a list of datapoints in Price_history.V2.t_of_yojson"
