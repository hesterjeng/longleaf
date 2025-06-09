module Array1 = Bigarray.Array1
module Array2 = Bigarray.Array2

type float64_elt = Bigarray.float64_elt
type c_layout = Bigarray.c_layout

let c_layout = Bigarray.c_layout
let float64 = Bigarray.float64

type t = {
  data : (float, float64_elt, c_layout) Array2.t;
  current : int;
  size : int;
  indicators_computed : bool;
}
(* [@@deriving show] *)

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

(* let pp_array : (float, float64_elt, c_layout) Array2.t Format.printer = *)
(*   invalid_arg "nyi" *)

let length x = Array2.dim2 x.data
let current x = x.current

(* let item_of_column x i : (Item.t, Error.t) result = *)
(*   match *)

(* let items (x : t) = *)

(* let collect_column x i = *)
(*   match i < x.size && i >= 0 with *)
(*   | false -> Error.fatal "Price_history.collect_column: illegal column access" *)
(*   | true -> *)

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

let get (res : t) (x : Row.t) i =
  let res = Array2.get res.data (Row.to_int x) i in
  assert (not @@ Float.is_nan res);
  res

let get_top (res : t) (x : Row.t) =
  let res = get res x @@ res.current in
  assert (not @@ Float.is_nan res);
  res

let item_of_column x i =
  let open Item in
  {
    timestamp =
      Ptime.of_float_s @@ get x Time i
      |> Option.get_exn_or
           "Expected to get a valid time in Price_history.item_of_column";
    open_ = get x Open i;
    high = get x High i;
    low = get x Low i;
    close = get x Close i;
    last = get x Last i;
    volume = get x Volume i |> Float.to_int;
  }

let to_items (x : t) =
  let rec aux i acc =
    match i >= x.size with
    | true -> acc
    | false ->
      let item = item_of_column x i in
      aux (i + i) @@ (item :: acc)
  in
  aux 0 []

let make size : t =
  {
    data = Array2.init float64 c_layout Row.count size (fun _ _ -> Float.nan);
    current = 0;
    size;
    indicators_computed = false;
  }

let copy (x : t) =
  let r = make x.size in
  Array2.blit x.data r.data;
  { r with current = x.current; indicators_computed = x.indicators_computed }

let set_item (x : t) (i : int) (item : Item.t) =
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

let add_item (x : t) (item : Item.t) =
  let ( let* ) = Result.( let* ) in
  let* () = set_item x x.current item in
  Result.return @@ { x with current = x.current + 1 }

let of_items size (l : Item.t list) =
  let ( let* ) = Result.( let* ) in
  let matrix = make size in
  let sorted = List.sort Item.compare l in
  let* res =
    List.fold_left
      (fun acc item ->
        let* acc = acc in
        add_item acc item)
      (Ok matrix) sorted
  in
  Result.return res

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
        let* acc = add_item acc current in
        aux (i + 1) xs @@ Result.return acc
    in
    let* res = aux 0 items (Ok res) in
    Result.return res
  | _ ->
    Error.json "Expected a list of datapoints in Price_history.V2.t_of_yojson"
