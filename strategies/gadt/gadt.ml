module Error = Longleaf_core.Error
module Signal = Longleaf_core.Signal
module Instrument = Longleaf_core.Instrument
module State = Longleaf_state
module Backend = Longleaf_backend
module Bars = Longleaf_bars
module Util = Longleaf_util
module Order = Longleaf_core.Order
module Data = Bars.Data
module Time = Longleaf_core.Time
module Options = Longleaf_core.Options
module Template = Longleaf_template

type const = VFloat of float | VInt of int
(* type env = (Uuidm.t, _ const) List.Assoc.t *)

module Type = struct
  type _ t =
    | Bool : bool t
    | Float : float t
    | Int : int t
    | Data : Data.Type.t t
    | Tacaml : Tacaml.Indicator.t t

  type shadow = A : _ t -> shadow

  let shadow x = A x
end

type context = {
  instrument : Instrument.t;
  data : Data.t;
  index : int;
  orders : Order.t list;  (* Order history for this instrument - for position risk management *)
}

(* GADT AST with phantom types for compile-time type safety *)
type _ t =
  | Const : 'a * 'a Type.t -> 'a t
  (* Type-safe data access *)
  | Data : Data.Type.t t -> float t
  (* | Indicator : Tacaml.Indicator.t t -> float t *)
  | App1 : ('a -> 'b) t * 'a t -> 'b t
  | App2 : ('a -> 'b -> 'c) t * 'a t * 'b t -> 'c t
  | App3 : ('a -> 'b -> 'c -> 'd) t * 'a t * 'b t * 'c t -> 'd t
  | Fun : string * ('a -> 'b) -> ('a -> 'b) t
  | ContextModifier : 'a t * (context -> 'a -> context) * 'b t -> 'b t
  | Var : Uuidm.t * 'a Type.t -> 'a t
  | Symbol : unit -> Instrument.t t
  (* Position risk management nodes *)
  | EntryPrice : float t
  | EntryTick : int t
  | TicksHeld : int t
  | HasPosition : bool t

let data x = Data (Const (x, Data))
let close = data Data.Type.Close
let volume = data @@ Data.Type.Volume
let index = data @@ Data.Type.Index
let last = data @@ Data.Type.Last
let open_ = data @@ Data.Type.Open
let high = data @@ Data.Type.High
let low = data @@ Data.Type.Low

(* Helper: Find entry (Buy) order from the order list *)
let find_entry_order orders =
  List.find_opt (fun (order : Order.t) ->
    match order.side with Buy -> true | Sell -> false) orders

(* Type-safe evaluation *)
let rec eval : type a. context -> a t -> (a, Error.t) result =
 fun ({ instrument; data; index; orders } as context) t ->
  let ( let* ) = Result.( let* ) in
  (* Bounds checking *)
  if index < 0 || index >= Data.length data then
    Error.fatal
      (Printf.sprintf "GADT.eval: index %d out of bounds (data length: %d)"
         index (Data.length data))
  else
    match t with
    | Const (x, _) -> Result.return x
    | Fun (_, f) -> Result.return f
    | ContextModifier (x, f, expr) ->
      let* arg = eval context x in
      let context = f context arg in
      let* res = eval context expr in
      Result.return res
    | App1 (f, x) ->
      let* f = eval context f in
      let* arg = eval context x in
      let res = f arg in
      Result.return res
    | App2 (f, x, y) ->
      let* f = eval context f in
      let* x = eval context x in
      let* y = eval context y in
      let res = f x y in
      Result.return res
    | App3 (f, x, y, z) ->
      let* f = eval context f in
      let* x = eval context x in
      let* y = eval context y in
      let* z = eval context z in
      let res = f x y z in
      Result.return res
    | Symbol () -> Result.return instrument
    | Var _ -> invalid_arg "Cannot evalute gadts with variables in them"
    | Data ty ->
      let* ty = eval context ty in
      Error.guard (Error.fatal "Error in GADT evaluation at Data node")
      @@ fun () -> Data.get data ty index
    | EntryPrice ->
      (match find_entry_order orders with
      | Some order -> Result.return order.price
      | None -> Error.fatal "No entry order found for EntryPrice")
    | EntryTick ->
      (match find_entry_order orders with
      | Some order -> Result.return order.tick
      | None -> Error.fatal "No entry order found for EntryTick")
    | TicksHeld ->
      (match find_entry_order orders with
      | Some order -> Result.return (index - order.tick)
      | None -> Result.return 0)  (* No position = 0 ticks held *)
    | HasPosition ->
      Result.return (not (List.is_empty orders))
(* | Indicator ty -> *)
(*   let* ty = eval context ty in *)
(*   Error.guard (Error.fatal "Error in GADT evaluation at Data node") *)
(*   @@ fun () -> Data.get data (Tacaml ty) index *)

module Subst = struct
  module Bindings = struct
    include Map.Make (Uuidm)

    let get id map =
      get id map |> function
      | Some x -> Ok x
      | None -> Error.fatal "No binding for variable"
  end

  let rec collect_variables : type a. a t -> 'b list = function
    | Var (id, ty) -> [ (id, Type.A ty) ]
    | Const _ -> []
    | Symbol _ -> []
    | Data x -> collect_variables x
    | ContextModifier (expr, _, x) ->
      collect_variables expr @ collect_variables x
    (* | Indicator x -> collect_variables x *)
    | App1 (f, x) -> collect_variables f @ collect_variables x
    | App2 (f, x, y) ->
      collect_variables f @ collect_variables x @ collect_variables y
    | App3 (f, x, y, z) ->
      collect_variables f @ collect_variables x @ collect_variables y
      @ collect_variables z
    | Fun _ -> []
    | EntryPrice -> []
    | EntryTick -> []
    | TicksHeld -> []
    | HasPosition -> []

  let collect_variables : 'a t -> (Uuidm.t * Type.shadow) list =
   fun x ->
    let vars = collect_variables x in
    List.uniq ~eq:(fun (id0, _) (id1, _) -> Uuidm.equal id0 id1) vars

  type env = { float_map : float Bindings.t; int_map : int Bindings.t }

  (* let of_l l = *)
  (*   let init = { *)
  (*     float_map = Bindings.empty; *)
  (*     int_map = Bindings.empty; *)
  (*   } in *)

  let env_of_arr params (arr : (Uuidm.t * Type.shadow) array) =
    Array.foldi
      (fun env i (id, Type.A ty) ->
        match ty with
        | Type.Bool -> invalid_arg "Gadt.env_of_arr Type.Bool"
        | Type.Data -> invalid_arg "Type.Data NYI (gadt.ml)"
        | Type.Tacaml -> invalid_arg "Type.Tacaml NYI (gadt.ml)"
        | Type.Float ->
          { env with float_map = Bindings.add id params.(i) env.float_map }
        | Type.Int ->
          let int_val = Int.of_float params.(i) in
          { env with int_map = Bindings.add id int_val env.int_map })
      { float_map = Bindings.empty; int_map = Bindings.empty }
      arr

  let rec instantiate : type a. env -> a t -> (a t, Error.t) result =
    let ( let* ) = Result.( let* ) in
    fun env -> function
      | Var (id, ty) ->
        (match ty with
        | Bool -> invalid_arg "NYI Instnatiate bool"
        | Data -> invalid_arg "NYI Instantiate Data variable"
        | Tacaml -> invalid_arg "NYI Instantiate Tacaml variable"
        | Float ->
          (match Bindings.get id env.float_map with
          | Ok res -> Result.return @@ Const (res, Float)
          | Error e -> Error e)
        | Int ->
          (match Bindings.get id env.int_map with
          | Ok res -> Result.return @@ Const (res, Int)
          | Error e -> Error e))
      | Const _ as x -> Result.return x
      | Fun _ as x -> Result.return x
      | Symbol () -> Result.return (Symbol ())
      | App1 (f, x) ->
        let* f' = instantiate env f in
        let* x' = instantiate env x in
        Result.return @@ App1 (f', x')
      | App2 (f, x, y) ->
        let* f' = instantiate env f in
        let* x' = instantiate env x in
        let* y' = instantiate env y in
        Result.return @@ App2 (f', x', y')
      | App3 (f, x, y, z) ->
        let* f' = instantiate env f in
        let* x' = instantiate env x in
        let* y' = instantiate env y in
        let* z' = instantiate env z in
        Result.return @@ App3 (f', x', y', z')
      | Data ty ->
        let* ty' = instantiate env ty in
        Result.return @@ Data ty'
      | ContextModifier (arg, f, x) ->
        let* ty' = instantiate env x in
        let* arg' = instantiate env arg in
        Result.return @@ ContextModifier (arg', f, ty')
      | EntryPrice -> Result.return EntryPrice
      | EntryTick -> Result.return EntryTick
      | TicksHeld -> Result.return TicksHeld
      | HasPosition -> Result.return HasPosition
  (* | Indicator ty -> *)
  (*   let* ty' = instantiate env ty in *)
  (*   Result.return @@ Indicator ty' *)

  (* let apply_subst (x : 'a expr) = *)
  (*   match collect_variables x with *)
  (*   | [] -> *)
end

(* Pretty printer for GADT expressions *)
let rec pp : type a. Format.formatter -> a t -> unit =
 fun fmt expr ->
  match expr with
  | Const (x, s) ->
    (match s with
    | Type.Bool -> Format.fprintf fmt "%b" x
    | Type.Data -> Format.fprintf fmt "%a" Data.Type.pp x
    | Type.Tacaml -> Format.fprintf fmt "%a" Tacaml.Indicator.pp x
    | Type.Float -> Format.fprintf fmt "%f" x
    | Type.Int -> Format.fprintf fmt "%d" x (* Format.fprintf fmt "Const(?)" *))
  | Fun (f, _) -> Format.fprintf fmt "%s" f
  | Symbol () -> Format.fprintf fmt "Symbol()"
  | ContextModifier (f, _, z) ->
    Format.fprintf fmt "ContextModifier(%a,%a)" pp f pp z
  | Var (id, ty) ->
    let ty_str =
      match ty with
      | Type.Bool -> "Bool Var"
      | Type.Data -> "Data.Type.t Var"
      | Type.Tacaml -> "Tacaml.Indicator.t Var"
      | Type.Float -> "Float Var"
      | Type.Int -> "Int Var"
    in
    Format.fprintf fmt "Var(%s:%s)" (Uuidm.to_string id) ty_str
  | Data e -> Format.fprintf fmt "(@[%a@])" pp e
  (* | Indicator e -> Format.fprintf fmt "Indicator(@[%a@])" pp e *)
  | App1 (Fun ("tacaml", _), x) -> Format.fprintf fmt "@[%a@]" pp x
  | App2 (Fun ("&&.", _), x, y) ->
    Format.fprintf fmt "(@[%a@ &&.@ %a@])" pp x pp y
  | App2 (Fun ("||", _), x, y) ->
    Format.fprintf fmt "(@[%a@ ||.@ %a@])" pp x pp y
  | App2 (Fun ("<.", _), x, y) ->
    Format.fprintf fmt "(@[%a@ <.@ %a@])" pp x pp y
  | App2 (Fun (">.", _), x, y) ->
    Format.fprintf fmt "(@[%a@ >.@ %a@])" pp x pp y
  | EntryPrice -> Format.fprintf fmt "EntryPrice"
  | EntryTick -> Format.fprintf fmt "EntryTick"
  | TicksHeld -> Format.fprintf fmt "TicksHeld"
  | HasPosition -> Format.fprintf fmt "HasPosition"
  | App1 (f, x) -> Format.fprintf fmt "@[%a@ %a@]" pp f pp x
  | App2 (f, x, y) -> Format.fprintf fmt "@[%a@ %a@ %a@]" pp f pp x pp y
  | App3 (f, x, y, z) ->
    Format.fprintf fmt "@[%a@ %a@ %a@ %a@]" pp f pp x pp y pp z

(* Helper to print expression to string *)
let to_string : type a. a t -> string = fun expr -> Format.asprintf "%a" pp expr

(* Helper to print variables found in expression *)
let debug_variables : type a. a t -> unit =
 fun expr ->
  let vars = Subst.collect_variables expr in
  Eio.traceln "=== VARIABLES IN EXPRESSION ===";
  Eio.traceln "Expression: %s" (to_string expr);
  Eio.traceln "Variables found: %d" (List.length vars);
  List.iteri
    (fun i (id, Type.A ty) ->
      let ty_str =
        match ty with
        | Type.Float -> "Float"
        | Type.Int -> "Int"
        | Type.Bool -> "Data"
        | Type.Data -> "Data"
        | Type.Tacaml -> "Tacaml"
      in
      Eio.traceln "  [%d] %s: %s" i (Uuidm.to_string id) ty_str)
    vars;
  Eio.traceln "================================"

(* Convenience operators *)
let ( >. ) e1 e2 = App2 (Fun (">.", ( >. )), e1, e2)
let ( <. ) e1 e2 = App2 (Fun ("<.", ( <. )), e1, e2)
let ( >=. ) e1 e2 = App2 (Fun (">=.", ( >=. )), e1, e2)
let ( <=. ) e1 e2 = App2 (Fun ("<=.", ( <=. )), e1, e2)
let ( =. ) e1 e2 = App2 (Fun ("=.", Float.equal), e1, e2)
let ( &&. ) e1 e2 = App2 (Fun ("&&.", ( && )), e1, e2)
let ( ||. ) e1 e2 = App2 (Fun ("||.", ( || )), e1, e2)
let ( +. ) e1 e2 = App2 (Fun ("+.", ( +. )), e1, e2)
let ( -. ) e1 e2 = App2 (Fun ("-.", ( -. )), e1, e2)
let ( *. ) e1 e2 = App2 (Fun ("*.", ( *. )), e1, e2)
let ( /. ) e1 e2 = App2 (Fun ("/.", ( /. )), e1, e2)

let conjunction (l : bool t list) =
  match l with
  | [] -> Const (true, Type.Bool)
  | x :: xs -> List.fold_left (fun acc gadt -> acc &&. gadt) x xs

let disjunction (l : bool t list) =
  match l with
  | [] -> Const (false, Type.Bool)
  | x :: xs -> List.fold_left (fun acc gadt -> acc &&. gadt) x xs

(* Additional operators *)
let not_ e = App1 (Fun ("not", not), e)

(* Domain-specific functions using ContextModifier *)

(* Lag function: access data N periods ago *)
let lag expr periods =
  ContextModifier
    ( Const (periods, Int),
      (fun ctx periods -> { ctx with index = ctx.index - periods }),
      expr )

(* Crossover detection functions *)
let cross_up e1 e2 =
  (* Current: e1 > e2 *)
  e1 >. e2
  &&.
  (* Previous: e1 <= e2 *)
  (lag e1 1 <=. lag e2 1)

let cross_down e1 e2 =
  (* Current: e1 < e2 *)
  e1 <. e2
  &&.
  (* Previous: e1 >= e2 *)
  (lag e1 1 >=. lag e2 1)

(* Placeholder functions for options-specific expressions *)
exception SpecialFunction of string

let moneyness_fn = fun _ _ -> raise (SpecialFunction "moneyness")
let days_to_expiry_fn = fun _ -> raise (SpecialFunction "days_to_expiry")

let moneyness underlying option =
  App2 (Fun ("moneyness", moneyness_fn), underlying, option)

let days_to_expiry option = App1 (Fun ("dte", days_to_expiry_fn), option)
