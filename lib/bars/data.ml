module Array1 = Bigarray.Array1
module Array2 = Bigarray.Array2

type data_matrix = (float, Bigarray.float64_elt, Bigarray.c_layout) Array2.t
type int_matrix = (int32, Bigarray.int32_elt, Bigarray.c_layout) Array2.t

module Type = struct
  let hash_fold_string = Ppx_hash_lib.Std.Hash.fold_string

  type t =
    | Index
    | Time
    | Last
    | Open
    | High
    | Low
    | Close
    | Volume
    | Tacaml of Tacaml.Indicator.t
    | Other of string
  [@@deriving variants, show { with_path = false }, eq, hash]

  let tacaml x = Tacaml x

  let show x =
    match x with
    | Tacaml x -> Tacaml.Indicator.show x
    | _ -> show x

  let is_int_ty (x : t) =
    match x with
    | Tacaml (I _) -> true
    | _ -> false

  (* let count = List.length Variants.descriptions *)
end

module Index = struct
  module IndexTable = Hashtbl.Make (Type)

  type table = int IndexTable.t
  type t = { mutable next_float : int; mutable next_int : int; tbl : table }

  let copy x =
    {
      next_float = x.next_float;
      next_int = x.next_int;
      tbl = IndexTable.copy x.tbl;
    }

  (* Given a data type, look up the row of the correct matrix to find it. Can return a row *)
  (* of either the int matrix or the float matrix. *)
  let get (x : t) (ty : Type.t) : int =
    match IndexTable.find_opt x.tbl ty with
    | None ->
      let next =
        match Type.is_int_ty ty with
        | true ->
          let res = x.next_int in
          x.next_int <- res + 1;
          res
        | false ->
          let res = x.next_float in
          x.next_float <- res + 1;
          res
      in
      IndexTable.replace x.tbl ty next;
      next
    | Some i -> i

  let make () = { next_float = 0; next_int = 0; tbl = IndexTable.create 200 }
end

type t = {
  data : data_matrix;
  int_data : int_matrix;
  index : Index.t;
  current : int;
  size : int;
  indicators_computed : bool;
  orders : Order.t list array;
}

type data = t

let size x = x.size
let current x = x.current

let get_ source row i =
  try Array2.get source row i with
  | e ->
    Eio.traceln "data.ml.get_: Index out of bounds!";
    raise e

let set_ source row i value =
  try Array2.set source row i value with
  | e ->
    Eio.traceln "data.ml.set_: Index out of bounds!";
    raise e

exception NaNInData of int * Type.t

let get (data : t) (ty : Type.t) i =
  assert (i >= 0);
  assert (i < data.size);
  let row = Index.get data.index ty in
  let res =
    match ty with
    | _ when Type.is_int_ty ty ->
      (* Float.of_int @@ *)
      let res = get_ data.int_data row i in
      Int32.to_float res
    | _ -> get_ data.data row i
  in
  match Float.is_nan res with
  | true ->
    let time_row = Index.get data.index Time in
    let time =
      get_ data.data time_row i |> Ptime.of_float_s |> function
      | Some x -> x
      | None -> Ptime.min
    in
    Eio.traceln "data.ml: (%a) raising exn %a index %d NaN (size %d)" Ptime.pp
      time Type.pp ty i data.size;
    raise (NaNInData (i, ty))
  | false -> res

let set (data : t) (ty : Type.t) i f =
  assert (i >= 0);
  assert (i < data.size);
  let row = Index.get data.index ty in
  let () =
    match ty with
    | _ when Type.is_int_ty ty -> set_ data.int_data row i @@ Int32.of_float f
    | _ -> set_ data.data row i f
  in
  ()

module Column = struct
  type t = { data : data; index : int }

  let of_data (x : data) i : (t, Error.t) result =
    if i >= 0 && i < x.size then Result.return { data = x; index = i }
    else
      Error.fatal
      @@ Format.sprintf "Data.LogicalColumn.of_data: index %d out of bounds" i

  let get (x : t) (ty : Type.t) =
    let err = Error.fatal "Data.LogicalColumn.get" in
    Error.guard err @@ fun () -> get x.data ty x.index

  let pp : t Format.printer =
   fun fmt _ -> Format.fprintf fmt "@[<Data.Column.t opaque>@]"
  (* let pp : t Format.printer = *)
  (*  fun fmt x -> *)
  (*   let r = Int.range' 0 Type.count in *)
  (*   let pp_pair = Pair.pp ~pp_sep:(Format.return ": ") Type.pp Float.pp in *)
  (*   let pp = Iter.pp_seq ~sep:"; " pp_pair in *)
  (*   let l = *)
  (*     Iter.map *)
  (*       (fun i -> *)
  (*         (\* let type_ = Type.of_int i in *\) *)
  (*         (type_, get_top x.data type_ i)) *)
  (*       r *)
  (*   in *)
  (*   Format.fprintf fmt "@[{ %a }@]@." pp l *)

  let set_exn (col : t) (ty : Type.t) value = set col.data ty col.index value

  let set (col : t) (ty : Type.t) value =
    let err = Error.fatal "Data.LogicalColumn.set" in
    Error.guard err @@ fun () -> set_exn col ty value

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
  type t = Tacaml.Safe.float_ba
  type introw = Tacaml.Safe.int_ba

  let slice start length array : t = Array1.sub array start length

  let get_time x i =
    Array1.get x i |> Ptime.of_float_s |> function
    | Some x -> Ok x
    | None -> Error.fatal "Invalid time in row of Data.t"
end

let get_row (data : t) (x : Type.t) =
  let ( let* ) = Result.( let* ) in
  let* source =
    match x with
    | Tacaml (I _) ->
      Eio.traceln "%a" Type.pp x;
      Error.fatal "Data.get_row: not a float row"
    | _ -> Ok data.data
  in
  Result.return @@ Array2.slice_left source @@ Index.get data.index x

let get_int_row (data : t) (x : Type.t) =
  let ( let* ) = Result.( let* ) in
  let* source =
    match x with
    | Tacaml (I _) -> Result.return data.int_data
    | _ -> Error.fatal "Data.get_int_row: not an int row"
  in
  Result.return @@ Array2.slice_left source @@ Index.get data.index x

let pp : t Format.printer =
 fun fmt (x : t) ->
  let a = Array.init x.size @@ fun i -> Column.of_data x i |> Result.get_exn in
  Format.fprintf fmt "@[{ %a }@]@." (Array.pp Column.pp) a

(* let pp_row (ty : Type.t) : t Format.printer = *)
(*  fun fmt (x : t) -> *)
(*   let bigarr = Array2.slice_left x.data @@ Type.to_int ty in *)
(*   let i = Array1.dim bigarr in *)
(*   let arr = Array.init i (fun i -> bigarr.{i}) in *)
(*   Format.fprintf fmt "%a" (Array.pp Float.pp) arr *)

let length x = Array2.dim2 x.data
let current x = x.current

let get_top (res : t) (x : Type.t) =
  let res = get res x @@ res.current in
  res

let get_top_int (res : t) (x : Type.t) =
  assert (res.current >= 0);
  assert (res.current < res.size);
  match x with
  | Tacaml (I _) ->
    let row = Index.get res.index x in
    Int32.to_int @@ get_ res.int_data row res.current
  | _ -> invalid_arg "get_top_int: not an integer indicator"

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
  try
    Result.return
    @@
    let rec aux i acc =
      match i >= x.size with
      | true -> acc
      | false ->
        let item = item_of_column x i in
        aux (i + 1) @@ (item :: acc)
    in
    aux 0 []
  with
  | NaNInData (i, ty) ->
    let msg = Format.asprintf "NaN in data %a %d" Type.pp ty i in
    Error.missing_data msg

let make size : t =
  {
    data =
      Array2.init Bigarray.float64 Bigarray.c_layout 150 size (fun _ _ ->
          Float.nan);
    int_data =
      Array2.init Bigarray.int32 Bigarray.c_layout 70 size (fun _ _ ->
          Int32.zero);
    current = 0;
    index = Index.make ();
    size;
    indicators_computed = false;
    orders = Array.make size [];
  }

let copy (x : t) =
  let r = make x.size in
  Array2.blit x.data r.data;
  Array2.blit x.int_data r.int_data;
  Array.blit x.orders 0 r.orders 0 x.size;
  {
    (* r with *)
    r
    with
    current = x.current;
    indicators_computed = x.indicators_computed;
    index = Index.copy x.index;
  }

let grow (x : t) =
  let new_size = x.size * 2 in
  let new_data =
    Array2.init Bigarray.float64 Bigarray.c_layout 150 new_size (fun _ _ ->
        Float.nan)
  in
  let new_int_data =
    Array2.init Bigarray.int32 Bigarray.c_layout 70 new_size (fun _ _ ->
        Int32.zero)
  in
  (* Create new orders array and copy existing orders *)
  let new_orders = Array.make new_size [] in
  Array.blit x.orders 0 new_orders 0 x.size;
  (* Copy existing data to new matrices *)
  for i = 0 to Array2.dim1 x.data - 1 do
    for j = 0 to Array2.dim2 x.data - 1 do
      Array2.set new_data i j (Array2.get x.data i j)
    done
  done;
  for i = 0 to Array2.dim1 x.int_data - 1 do
    for j = 0 to Array2.dim2 x.int_data - 1 do
      Array2.set new_int_data i j (Array2.get x.int_data i j)
    done
  done;
  {
    data = new_data;
    int_data = new_int_data;
    index = Index.copy x.index;
    current = x.current;
    size = new_size;
    indicators_computed = x.indicators_computed;
    orders = new_orders;
  }

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
  let size = List.length l in
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

let is_sorted (x : t) =
  let ( let* ) = Result.( let* ) in
  let* row = get_row x Time in
  let* () =
    Seq.fold_left
      (fun acc i ->
        let* _ = acc in
        let* curr = Row.get_time row i in
        let* prev = Row.get_time row (i - 1) in
        match Ptime.compare curr prev with
        | 1 -> Ok ()
        | _ -> Error.fatal "Bad timestamp comparison in Data.sort")
      (Ok ())
    @@ Seq.take (length x - 1) (Seq.ints 1)
  in
  Result.return ()

let t_of_yojson ?symbol (json : Yojson.Safe.t) : (t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  match json with
  | `List items ->
    let _ = symbol in
    (* Eio.traceln "Data.t_of_yojson symbol: %a" (Option.pp String.pp) symbol; *)
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
    let* () = is_sorted res in
    Result.return res
  | _ ->
    Error.json "Expected a list of datapoints in Price_history.V2.t_of_yojson"

let yojson_of_t (x : t) : Yojson.Safe.t =
  let items =
    to_items x |> function
    | Ok x -> x
    | Error e ->
      Eio.traceln "Data.yojson_of_t: %a" Error.pp e;
      invalid_arg "Data.yojson_of_t"
  in
  let l = List.map Item.yojson_of_t items in
  `List l

let set_current x current = { x with current }

(* Order management functions *)
let add_order (data : t) (tick : int) (order : Order.t) =
  if tick >= 0 && tick < data.size then (
    data.orders.(tick) <- order :: data.orders.(tick);
    Ok ()
  ) else
    Error.fatal "Data.add_order: tick index out of bounds"

let get_orders (data : t) (tick : int) =
  if tick >= 0 && tick < data.size then
    Ok data.orders.(tick)
  else
    Error.fatal "Data.get_orders: tick index out of bounds"

let get_all_orders (data : t) = data.orders
