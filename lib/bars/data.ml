module Array1 = Bigarray.Array1
module Array2 = Bigarray.Array2

type data_matrix = (float, Bigarray.float64_elt, Bigarray.c_layout) Array2.t
type int_matrix = (int, Bigarray.int_elt, Bigarray.c_layout) Array2.t

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
}

type data = t

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

let get (data : t) (ty : Type.t) i =
  assert (i >= 0);
  assert (i < data.size);
  let row = Index.get data.index ty in
  let res =
    match ty with
    | _ when Type.is_int_ty ty -> Float.of_int @@ get_ data.int_data row i
    | _ -> get_ data.data row i
  in
  assert (
    match not @@ Float.is_nan res with
    | false ->
      Eio.traceln "%a index %d NaN" Type.pp ty i;
      (* Eio.traceln "%a" Data. *)
      false
      (* let col = Column.of_data data i in *)
      (* Eio.traceln "%a" (Result.pp' Column.pp Error.pp) col; *)
    | true -> true);
  res

let set (data : t) (ty : Type.t) i f =
  assert (i >= 0);
  assert (i < data.size);
  let row = Index.get data.index ty in
  let () =
    match ty with
    | _ when Type.is_int_ty ty -> set_ data.int_data row i @@ Int.of_float f
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
  type t = (float, Bigarray.float64_elt, Bigarray.c_layout) Array1.t
  type introw = (int, Bigarray.int_elt, Bigarray.c_layout) Array1.t

  let slice start length array : t = Array1.sub array start length
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
    get_ res.int_data row res.current
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
  let rec aux i acc =
    match i >= x.size with
    | true -> acc
    | false ->
      let item = item_of_column x i in
      aux (i + 1) @@ (item :: acc)
  in
  aux 0 []

let make size : t =
  {
    data =
      Array2.init Bigarray.float64 Bigarray.c_layout 150 size (fun _ _ ->
          Float.nan);
    int_data = Array2.init Bigarray.int Bigarray.c_layout 70 size (fun _ _ -> 0);
    current = 0;
    index = Index.make ();
    size;
    indicators_computed = false;
  }

let copy (x : t) =
  let r = make x.size in
  Array2.blit x.data r.data;
  Array2.blit x.int_data r.int_data;
  {
    (* r with *)
    r
    with
    current = x.current;
    indicators_computed = x.indicators_computed;
    index = Index.copy x.index;
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

let set_current x current = { x with current }
