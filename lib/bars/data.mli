(** This module provides a data structure for storing and accessing time-series
    data, such as stock prices. It uses a Bigarray to efficiently store the data
    in a contiguous block of memory. *)

(** {1 Types} *)

type data_matrix =
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t
(** The underlying data structure for the time-series data, which is a 2D
    Bigarray of 64-bit floats. *)

type int_matrix = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array2.t

module Index : sig
  type t
end

type t = {
  data : data_matrix;
  (* talib_indicators : data_matrix; *)
  (* other_indicators : data_matrix; *)
  int_data : int_matrix;
  index : Index.t;
  (* custom : Custom.t; *)
  current : int;
  size : int;
  indicators_computed : bool;
}
(** The main data type for the time-series data. It contains the data matrix,
    the current index, the size of the data, and a flag indicating whether
    indicators have been computed. *)

type data = t

(** {1 Data Types} *)

module Type : sig
  (** This module defines the different types of data that can be stored in the
      data matrix. *)

  (** The type for the different data fields. *)
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
    (* | CustomTacaml of Tacaml.t *)
    | Other of string
  [@@deriving variants, show { with_path = false }]

  (* val to_int : t -> int *)
  (* (\** Converts a data field type to its integer representation. *\) *)

  (* val of_int : int -> t *)
  (* (\** Converts an integer to its corresponding data field type. *\) *)
end

(** {1 Accessing Data} *)

val get : t -> Type.t -> int -> float
(** [get data x i] returns the value of the data field [x] at index [i]. *)

(** {1 Column Operations} *)

module Column : sig
  (** This module provides functions for working with columns of the data
      matrix. *)

  type t = { data : data; index : int }
  (** The type for a column of the data matrix. *)

  val of_data : data -> int -> (t, Error.t) result
  (** [of_data x i] returns a column of the data matrix at index [i]. *)

  val get : t -> Type.t -> (float, Error.t) result
  (** [get x ty] returns the value of the data field [ty] in the column [x]. *)

  val pp : t Format.printer
  (** The pretty-printer for a column. *)

  val set : t -> Type.t -> float -> (unit, Error.t) result
  (** [set col ty value] sets the value of the data field [ty] in the column
      [col] to [value]. *)

  val set_exn : t -> Type.t -> float -> unit
  (** [set_exn col ty value] is the same as {!set}, but raises an exception on
      error. *)

  val timestamp : t -> (Ptime.t, Error.t) result
  (** [timestamp x] returns the timestamp of the column [x]. *)

  val last : t -> (float, Error.t) result
  (** [last x] returns the last price in the column [x]. *)

  val last_exn : t -> float
  (** [last_exn x] is the same as {!last}, but raises an exception on error. *)

  val open_ : t -> (float, Error.t) result
  (** [open_ x] returns the open price in the column [x]. *)

  val high : t -> (float, Error.t) result
  (** [high x] returns the high price in the column [x]. *)

  val low : t -> (float, Error.t) result
  (** [low x] returns the low price in the column [x]. *)

  val close : t -> (float, Error.t) result
  (** [close x] returns the close price in the column [x]. *)

  val volume : t -> (float, Error.t) result
  (** [volume x] returns the volume in the column [x]. *)
end

(** {1 Row Operations} *)

module Row : sig
  (** This module provides functions for working with rows of the data matrix.
  *)

  type t = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
  (** The type for a row of the data matrix. *)

  type introw = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

  val slice : int -> int -> t -> t
  (** [slice x y a] returns a slice of the row [a] from index [x] with length
      [y]. *)
end

val get_row : t -> Type.t -> (Row.t, Error.t) result
(** [get_row data x] returns the row of the data matrix corresponding to the
    data field [x]. *)

val get_int_row : t -> Type.t -> (Row.introw, Error.t) result
(** [get_row data x] returns the row of the data matrix corresponding to the
    data field [x]. *)

(** {1 Pretty-printers} *)

val pp : t Format.printer
(** The pretty-printer for the data matrix. *)

(* val pp_row : Type.t -> t Format.printer *)
(* (\** [pp_row ty] returns a pretty-printer for the row of the data matrix *)
(*     corresponding to the data field [ty]. *\) *)

(** {1 Metadata} *)

val length : t -> int
(** [length x] returns the number of data points in the data matrix. *)

val current : t -> int
(** [current x] returns the current index in the data matrix. *)

(** {1 Modifying Data} *)

val set : t -> Type.t -> int -> float -> unit
(** [set res x i value] sets the value of the data field [x] at index [i] to
    [value]. *)

(* val get_top : t -> Type.t -> float *)
(* (\** [get_top res x] returns the value of the data field [x] at the current *)
(*     index. *\) *)

(* val get_top_int : t -> Type.t -> int *)
(* (\** [get_top_int res x] returns the integer value of the data field [x] at the *)
(*     current index. Only works for integer indicators (Tacaml (I _)). *\) *)

(** {1 Conversions} *)

val item_of_column : t -> int -> Item.t
(** [item_of_column x i] returns the item at index [i] of the data matrix. *)

val to_items : t -> Item.t list
(** [to_items x] returns a list of all the items in the data matrix. *)

(** {1 Creating and Copying} *)

val make : int -> t
(** [make size] creates a new data matrix with the given size. *)

val copy : t -> t
(** [copy x] returns a copy of the data matrix [x]. *)

(** {1 Setting Data} *)

val set_item : t -> int -> Item.t -> (unit, Error.t) result
(** [set_item x i item] sets the item at index [i] of the data matrix to [item].
*)

val set_column : t -> int -> Column.t -> (unit, Error.t) result
(** [set_column x i column] sets the column at index [i] of the data matrix to
    [column]. *)

val add_item : t -> Item.t -> int -> (unit, Error.t) result
(** [add_item x item i] adds the item [item] to the data matrix at index [i]. *)

val of_items : Item.t list -> (t, Error.t) result
(** [of_items l] creates a new data matrix from the given list of items. *)

(** {1 JSON Serialization} *)

val load_json_item : t -> int -> Yojson.Safe.t -> (unit, Error.t) result
(** [load_json_item data i json] loads an item from the given JSON object and
    adds it to the data matrix at index [i]. *)

val t_of_yojson : Yojson.Safe.t -> (t, Error.t) result
(** [t_of_yojson json] creates a new data matrix from the given JSON object. *)

val yojson_of_t : t -> Yojson.Safe.t
(** [yojson_of_t x] returns a JSON representation of the data matrix [x]. *)
