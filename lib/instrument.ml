type t = Security of string | Contract of Contract.t [@@deriving show]

let symbol = function Security x -> x | Contract x -> x.symbol

let pp_string : t Format.printer =
 fun fmt x ->
  let str = symbol x in
  Format.fprintf fmt "%s" str

let equal x y = String.equal (symbol x) (symbol y)
let hash x = String.hash @@ symbol x
let yojson_of_t (x : t) : Yojson.Safe.t = `String (symbol x)

module Parsing : sig end = struct
  (* type p1 = *)
  (*   | Contract_parsing of (int * string) *)
  (*   | Regular of string *)

  type parsed = {
    symbol : string;
    underlying : string;
    yy : string;
    mm : string;
    dd : string;
    ty : string;
    strike : float option;
  }

  type wip = Regular of string | Contract of parsed

  let of_string (x : string) =
    let l = String.to_list x |> List.map Char.to_string in
    let first_int =
      List.find_mapi
        (fun i s -> match Int.of_string s with Some _ -> Some i | _ -> None)
        l
    in
    match first_int with
    | None -> Regular x
    | Some i ->
        let underlying, rest = String.take_drop i x in
        let underlying =
          String.filter (fun x -> not @@ Char.equal x ' ') underlying
        in
        let yy, rest = String.take_drop 2 rest in
        let mm, rest = String.take_drop 2 rest in
        let dd, rest = String.take_drop 2 rest in
        let ty, rest = String.take_drop 1 rest in
        let strike =
          String.drop_while (fun c -> not @@ Char.equal c '0') rest
          |> Float.of_string_opt
        in
        Contract { symbol = x; underlying; yy; mm; dd; ty; strike }
end

let t_of_yojson (_ : Yojson.Safe.t) = invalid_arg "Instrument.t_of_yojson NYI"
let security x = Security x
