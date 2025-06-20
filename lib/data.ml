module Array1 = Bigarray.Array1
module Array2 = Bigarray.Array2

type float64_elt = Bigarray.float64_elt
type c_layout = Bigarray.c_layout

let c_layout = Bigarray.c_layout
let fortran_layout = Bigarray.fortran_layout
let float64 = Bigarray.float64

type data_matrix = (float, float64_elt, c_layout) Array2.t

type t = {
  data : data_matrix;
  current : int;
  size : int;
  indicators_computed : bool;
}

type data = t
(* [@@deriving show] *)

(* type slice = (float, float64_elt, c_layout) Array1.t *)

module Type = struct
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
  [@@deriving variants, show]

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

  let of_int = function
    | 0 -> Index
    | 1 -> Time
    | 2 -> Last
    | 3 -> Open
    | 4 -> High
    | 5 -> Low
    | 6 -> Close
    | 7 -> Volume
    | 8 -> SMA
    | 9 -> FSO_K
    | 10 -> FSO_D
    | 11 -> RSI
    | _ -> invalid_arg "Invalid Data.Type.of_int"
end

module Column = struct
  type t = (float, float64_elt, c_layout) Array1.t

  let pp : t Format.printer =
   fun fmt x ->
    let r = Int.range' 0 Type.count in
    let l = Iter.map (fun i -> (Type.of_int i, Array1.get x i)) r in
    let pp_pair = Pair.pp Type.pp Float.pp in
    let pp = Iter.pp_seq ~sep:";@ " pp_pair in
    Format.fprintf fmt "@[%a@]@." pp l

  let of_data (x : data) i : (t, Error.t) result =
    let err = Error.fatal "Data.Column.of_data" in
    Error.guard err @@ fun () ->
    let matrix = Array2.change_layout x.data fortran_layout in
    let col = Array2.slice_right matrix @@ (i + 1) in
    let c_col = Array1.change_layout col c_layout in
    assert (Array1.dim c_col = Type.count);
    c_col

  let get (x : t) (ty : Type.t) =
    let err = Error.fatal "Data.Column.get" in
    Error.guard err @@ fun () ->
    let i = Type.to_int ty in
    Array1.get x i

  let set (col : t) (ty : Type.t) x =
    let err = Error.fatal "Data.Column.set" in
    Error.guard err @@ fun () ->
    let i = Type.to_int ty in
    Array1.set col i x

  let set_exn (col : t) (ty : Type.t) x =
    let i = Type.to_int ty in
    Array1.set col i x

  let timestamp (x : t) =
    let ( let* ) = Result.( let* ) in
    let* time_f = get x Time in
    match Ptime.of_float_s time_f with
    | Some t -> Result.return t
    | None -> Error.fatal "Illegal timestamp in Data.Column.timestamp"

  let last x = get x Last

  let last_exn (x : t) =
    let i = Type.to_int Last in
    try Array1.get x i with
    | e ->
      Eio.traceln "Error getting last price in Data.Column.last_exn";
      raise e

  let open_ x = get x Open
  let high x = get x High
  let low x = get x Low
  let close x = get x Close
  let volume x = get x Volume

  let of_item (x : Item.t) =
    let arr : t =
      Array1.init float64 c_layout Type.count (fun _ -> Float.nan)
    in
    let timestamp = Ptime.to_float_s x.timestamp in
    let volume = Float.of_int x.volume in
    set_exn arr Time timestamp;
    set_exn arr Volume volume;
    set_exn arr Open x.open_;
    set_exn arr High x.high;
    set_exn arr Low x.low;
    set_exn arr Close x.close;
    set_exn arr Last x.last;
    arr
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

(* Type 0 : i (index)*)
(* Type 1 : time (float) *)
(* Type 2 : price *)
(* Type 3 : open *)
(* Type 4 : high *)
(* Type 5 : low *)
(* Type 6 : close *)
(* Type 7:  volume *)
(* Type 8 : SMA *)
(* Type 9 : FSO_K *)
(* Type 10 : FSO_D *)
(* Type 11 : RSI *)

let set (res : t) (x : Type.t) i value =
  Array2.set res.data (Type.to_int x) i @@ value

let get (data : t) (x : Type.t) i =
  let res = Array2.get data.data (Type.to_int x) i in
  assert (i >= 0);
  assert (i < data.size);
  assert (
    match not @@ Float.is_nan res with
    | false ->
      Eio.traceln "%a index %d NaN" Type.pp x i;
      let col = Column.of_data data i in
      Eio.traceln "%a" (Result.pp' Column.pp Error.pp) col;
      false
    | true -> true);
  res

let get_top (res : t) (x : Type.t) =
  let res = get res x @@ res.current in
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
    data = Array2.init float64 c_layout Type.count size (fun _ _ -> Float.nan);
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
  | _ -> Error.fatal "Illegal index access in Price_history.V2.set_item"

let set_column (x : t) (i : int) (column : Column.t) =
  let ( let* ) = Result.( let* ) in
  let* timestamp = Column.timestamp column in
  let* last = Column.last column in
  let* open_ = Column.open_ column in
  let* high = Column.high column in
  let* low = Column.low column in
  let* close = Column.close column in
  let* volume = Column.volume column in
  set x Index i (Float.of_int i);
  set x Time i @@ Ptime.to_float_s timestamp;
  set x Last i last;
  set x Open i open_;
  set x High i high;
  set x Low i low;
  set x Close i close;
  set x Volume i volume;
  Result.return ()

let add_item (x : t) (item : Item.t) =
  let ( let* ) = Result.( let* ) in
  let* () = set_item x x.current item in
  Result.return @@ { x with current = x.current + 1 }

let add_column (x : t) (column : Column.t) =
  let ( let* ) = Result.( let* ) in
  let* () = set_column x x.current column in
  Result.return @@ { x with current = x.current + 1 }

let of_items (l : Item.t list) =
  let ( let* ) = Result.( let* ) in
  let size = 2 * List.length l in
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

let yojson_of_t (x : t) : Yojson.Safe.t =
  to_items x |> List.map Item.yojson_of_t |> fun l -> `List l
