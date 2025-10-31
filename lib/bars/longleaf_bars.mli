module Latest : sig
  type t [@@deriving show]

  val empty : unit -> t
  val get : t -> Instrument.t -> (Data.Column.t, Error.t) result
  val iter : (Instrument.t -> Data.Column.t -> unit) -> t -> unit
  val timestamp : t -> (Time.t, Error.t) result
  val of_seq : (Instrument.t * Data.Column.t) Seq.t -> t
  val set : t -> Instrument.t -> Data.Column.t -> unit
end

module Data = Data

type t

val get : t -> Instrument.t -> (Data.t, Error.t) result
val length : t -> (int, Error.t) result
val of_file : ?eio_env:Eio_unix.Stdenv.base -> string -> t
val of_seq : (Instrument.t * Data.t) Seq.t -> t
val of_list : (Instrument.t * Data.t) list -> t
val yojson_of_t : t -> (Yojson.Safe.t, Error.t) result

val t_of_yojson :
  ?eio_env:Eio_unix.Stdenv.base -> Yojson.Safe.t -> (t, Error.t) result

val empty : unit -> t
val copy : t -> t
val timestamp : t -> int -> (Time.t, Error.t) result

(* val append : Latest.t -> t -> (unit, Error.t) result *)
val pp : t Format.printer
val pp_stats : t Format.printer
val combine : t list -> (t, Error.t) result
val print_to_file : ?filename:string -> t -> string -> (unit, Error.t) result
val print_to_file_direct : t -> string -> (unit, Error.t) result
val fold : t -> 'a -> (Instrument.t -> Data.t -> 'a -> 'a) -> 'a
val grow : t -> (t, Error.t) result

(* List the bars json files in the data directory *)
val files : unit -> string list

(** Validation *)

val validate_no_nan : t -> (unit, Error.t) result
(** [validate_no_nan bars] validates that all symbols in bars contain no NaN values
    in essential price data types across all ticks. Returns an error if any NaN
    values are detected. This should be called after loading data to ensure data
    integrity before running strategies. *)

(* val last_bar : t -> (Latest.t, Error.t) result *)
(* val to_queue : t -> (Latest.t Queue.t, Error.t) result *)
