module Array1 = Bigarray.Array1
module Array2 = Bigarray.Array2

(* type float64_elt = Bigarray.float64_elt *)
(* type c_layout = Bigarray.c_layout *)

(* let c_layout = Bigarray.c_layout *)
(* let fortran_layout = Bigarray.fortran_layout *)
(* let float64 = Bigarray.float64 *)

type data_matrix = (float, Bigarray.float64_elt, Bigarray.c_layout) Array2.t

type t = {
  data : data_matrix;
  current : int;
  size : int;
  indicators_computed : bool;
}

type data = t

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
  [@@deriving variants, show { with_path = false }]

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

let get (data : t) (x : Type.t) i =
  let res =
    try Array2.get data.data (Type.to_int x) i with
    | e ->
      Eio.traceln "data.ml.get: Index (%d) out of bounds: len %d" i data.size;
      raise e
  in
  assert (i >= 0);
  assert (i < data.size);
  assert (
    match not @@ Float.is_nan res with
    | false ->
      Eio.traceln "%a index %d NaN" Type.pp x i;
      (* Eio.traceln "%a" Data. *)
      false
      (* let col = Column.of_data data i in *)
      (* Eio.traceln "%a" (Result.pp' Column.pp Error.pp) col; *)
    | true -> true);
  res

module Column = struct
  type t = { data : data_matrix; index : int }

  let of_data (x : data) i : (t, Error.t) result =
    if i >= 0 && i < x.size then Result.return { data = x.data; index = i }
    else
      Error.fatal
      @@ Format.sprintf "Data.LogicalColumn.of_data: index %d out of bounds" i

  let get (x : t) (ty : Type.t) =
    let err = Error.fatal "Data.LogicalColumn.get" in
    Error.guard err @@ fun () -> Array2.get x.data (Type.to_int ty) x.index

  let pp : t Format.printer =
   fun fmt x ->
    let r = Int.range' 0 Type.count in
    let pp_pair = Pair.pp ~pp_sep:(Format.return ": ") Type.pp Float.pp in
    let pp = Iter.pp_seq ~sep:"; " pp_pair in
    let l =
      Iter.map (fun i -> (Type.of_int i, Array2.get x.data i x.index)) r
    in
    Format.fprintf fmt "@[{ %a }@]@." pp l

  let set (col : t) (ty : Type.t) value =
    let err = Error.fatal "Data.LogicalColumn.set" in
    Error.guard err @@ fun () ->
    Array2.set col.data (Type.to_int ty) col.index value

  let set_exn (col : t) (ty : Type.t) value =
    Array2.set col.data (Type.to_int ty) col.index value

  let timestamp (x : t) =
    let ( let* ) = Result.( let* ) in
    let* time_f = get x Time in
    match Ptime.of_float_s time_f with
    | Some t -> Result.return t
    | None -> Error.fatal "Illegal timestamp in Data.LogicalColumn.timestamp"

  let last x = get x Last

  let last_exn (x : t) =
    (* try get x Last with *)
    (* | e -> *)
    match get x Last with
    | Ok x -> x
    | Error e ->
      Eio.traceln "Error getting last price in Data.LogicalColumn.last_exn";
      Eio.traceln "%a" Error.pp e;
      invalid_arg "Illegal get operation"

  let open_ x = get x Open
  let high x = get x High
  let low x = get x Low
  let close x = get x Close
  let volume x = get x Volume
end

module Row = struct
  type t = (float, Bigarray.float64_elt, Bigarray.c_layout) Array1.t

  let slice start length array : t = Array1.sub array start length
end

let get_row (data : t) (x : Type.t) : Row.t =
  Array2.slice_left data.data @@ Type.to_int x

let pp : t Format.printer =
 fun fmt (x : t) ->
  let a = Array.init x.size @@ fun i -> Column.of_data x i |> Result.get_exn in
  Format.fprintf fmt "@[{ %a }@]@." (Array.pp Column.pp) a

let pp_row (ty : Type.t) : t Format.printer =
 fun fmt (x : t) ->
  let bigarr = Array2.slice_left x.data @@ Type.to_int ty in
  let i = Array1.dim bigarr in
  let arr = Array.init i (fun i -> bigarr.{i}) in
  Format.fprintf fmt "%a" (Array.pp Float.pp) arr

let length x = Array2.dim2 x.data
let current x = x.current

let set (res : t) (x : Type.t) i value =
  try Array2.set res.data (Type.to_int x) i @@ value with
  | e ->
    Eio.traceln "data.ml.set: Index (%d) out of bounds: len %d" i res.size;
    raise e

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
    data =
      Array2.init Bigarray.float64 Bigarray.c_layout Type.count size (fun _ _ ->
          Float.nan);
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

let add_item (x : t) (item : Item.t) i =
  let ( let* ) = Result.( let* ) in
  let* () = set_item x i item in
  Result.return ()

(* let add_column (x : t) (column : Column.t) = *)
(*   let ( let* ) = Result.( let* ) in *)
(*   let* () = set_column x x.current column in *)
(*   Result.return () *)

let of_items (l : Item.t list) =
  let ( let* ) = Result.( let* ) in
  let size = 2 * List.length l in
  let matrix = make size in
  let sorted = List.sort Item.compare l in
  let* () =
    List.foldi
      (fun acc i item ->
        let* () = acc in
        add_item matrix item i)
      (Ok ()) sorted
  in
  Result.return matrix

let member = Yojson.Safe.Util.member
let float_member s x = Yojson.Safe.Util.member s x |> Yojson.Safe.Util.to_number
let int_member s x = Yojson.Safe.Util.member s x |> Yojson.Safe.Util.to_int

let load_json_item (data : t) i (json : Yojson.Safe.t) =
  try
    let time =
      member "t" json |> Yojson.Safe.Util.to_string |> Time.of_string
      |> Ptime.to_float_s
    in
    set data Time i time;
    set data Open i @@ float_member "o" json;
    set data High i @@ float_member "h" json;
    set data Low i @@ float_member "l" json;
    set data Close i @@ float_member "c" json;
    set data Last i @@ float_member "c" json;
    set data Volume i @@ Float.of_int @@ int_member "v" json;
    Result.return ()
  with
  | _ ->
    Eio.traceln "%a" Yojson.Safe.pp json;
    Error.fatal "Bad json in Data.load_json_item"

let t_of_yojson (json : Yojson.Safe.t) : (t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  match json with
  | `List items ->
    let size = List.length items in
    let res = make size in
    let* () =
      List.foldi
        (fun acc i json ->
          let* _ = acc in
          let* () = load_json_item res i json in
          Result.return ())
        (Ok ()) items
    in
    Result.return res
  | _ ->
    Error.json "Expected a list of datapoints in Price_history.V2.t_of_yojson"

let yojson_of_t (x : t) : Yojson.Safe.t =
  to_items x |> List.map Item.yojson_of_t |> fun l -> `List l
