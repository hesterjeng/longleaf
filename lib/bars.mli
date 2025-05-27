module Latest : sig
  type t [@@deriving show]

  val empty : unit -> t
  val get : t -> Instrument.t -> (Item.t, Error.t) result
  val iter : (Instrument.t -> Item.t -> unit) -> t -> unit
  val timestamp : t -> (Time.t, Error.t) result
  val of_seq : (Instrument.t * Item.t) Seq.t -> t
  val fold : t -> 'a -> (Instrument.t -> Item.t -> 'a -> 'a) -> 'a
  val set : t -> Instrument.t -> Item.t -> unit
end

type t [@@deriving show]

val pp_stats : t Format.printer
val get : t -> Instrument.t -> Item.t Vector.vector option
val fold : t -> 'a -> (Instrument.t -> Price_history.t -> 'a -> 'a) -> 'a

val map :
  (Instrument.t * Price_history.t -> Instrument.t * Price_history.t) -> t -> t

val of_seq : (Instrument.t * Price_history.t) Seq.t -> t
val append : Latest.t -> t -> unit
val print_to_file : ?filename:string -> t -> string -> unit
val print_to_file_direct : t -> string -> unit
val keys : t -> Instrument.t list
val iter : (Instrument.t -> Price_history.t -> unit) -> t -> unit
val t_of_yojson : Yojson.Safe.t -> (t, Error.t) result
val yojson_of_t : t -> Yojson.Safe.t
val combine : t list -> t
val of_file : string -> t
val length : t -> int
val sort : Item.t Ord.t -> t -> unit
val add_order : Order.t -> t -> (unit, Error.t) result
val length_check : t -> (int, Error.t) result
val copy : t -> t

val split :
  midpoint:int -> target_length:int -> combined_length:int -> t -> t * t

val empty : unit -> t
