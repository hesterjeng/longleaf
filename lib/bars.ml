module Hashtbl_make = Hashtbl.Make
module Hashtbl = Hashtbl_make (Instrument)

let fold (x : 'a Hashtbl.t) init f = Hashtbl.fold f x init

module Latest = Latest

type t = Data.t Hashtbl.t

let copy x =
  let copied = Hashtbl.copy x in
  Hashtbl.iter (fun s ph -> Hashtbl.replace copied s @@ Data.copy ph) x;
  copied

let t_of_yojson (json : Yojson.Safe.t) : (t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  let bars = Yojson.Safe.Util.member "bars" json in
  let assoc = Yojson.Safe.Util.to_assoc bars in
  let* mapped =
    Result.map_l
      (fun (symbol, json) ->
        let* instrument = Instrument.of_string_res symbol in
        let* ph = Data.t_of_yojson json in
        Result.return @@ (instrument, ph))
      assoc
  in
  let seq = Seq.of_list mapped in
  Result.return @@ Hashtbl.of_seq seq

let yojson_of_t (x : t) : Yojson.Safe.t =
  Eio.traceln "Bars.yojson_of_t NYI";
  `Null

let empty () = Hashtbl.create 100

let get (x : t) instrument =
  Hashtbl.find_opt x instrument |> function
  | Some x -> Result.return x
  | None -> Error.missing_data "Missing data for instrument in V2 bars"

let length (x : t) =
  let ( let* ) = Result.( let* ) in
  fold x (Ok 0) @@ fun _ ph acc ->
  let* acc = acc in
  let len = Data.length ph in
  match acc with
  | 0 -> Ok len
  | n when len = n -> Ok acc
  | _ -> Error.fatal "Mismated price history v2 matrices in Bars v2"

let of_file file =
  Yojson.Safe.from_file file |> t_of_yojson |> function
  | Ok x -> x
  | Error e -> invalid_arg @@ Error.show e

let append (latest : Latest.t) (x : t) : (unit, Error.t) result =
  let ( let* ) = Result.( let* ) in
  fold latest (Ok ()) @@ fun symbol item acc ->
  let* acc = acc in
  let* ph = get x symbol in
  let* appended = Data.add_item ph item in
  Hashtbl.replace x symbol appended;
  Result.return acc

let pp : t Format.printer =
 fun fmt _ -> Format.fprintf fmt "@[<Bars.pp opaque>@]@."

let pp_stats : t Format.printer =
 fun fmt b ->
  match length b with
  | Ok len -> Format.fprintf fmt "@[bars length: %d@]@." len
  | Error _ -> Format.fprintf fmt "@[Error when printing bars stats@]@."

let latest_i (x : t) i =
  let ( let* ) = Result.( let* ) in
  let res = Latest.empty () in
  let* () =
    fold x (Ok ()) @@ fun instrument ph acc ->
    let* _ = acc in
    let get = Data.get ph in
    let* timestamp =
      get Time i |> Ptime.of_float_s |> function
      | Some x -> Ok x
      | None -> Error.fatal "Illegal time in Bars.last_bar"
    in
    Latest.set res instrument
    @@ {
         timestamp;
         last = get Last i;
         open_ = get Open i;
         high = get High i;
         low = get Low i;
         close = get Close i;
         volume = Int.of_float @@ get Volume i;
       };
    Result.return ()
  in
  Result.return res

let to_queue (x : t) : (Latest.t Queue.t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  let q : Latest.t Queue.t = Queue.create () in
  let* len = length x in
  let* () =
    let r = Int.range' 0 len in
    Iter.fold
      (fun acc i ->
        let* _ = acc in
        let* latest = latest_i x i in
        Queue.add latest q;
        Result.return ())
      (Ok ()) r
  in
  Result.return q

let keys (x : t) = Hashtbl.to_seq_keys x |> Seq.to_list

let combine (l : t list) : (t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  let keys = List.flat_map keys l |> List.uniq ~eq:Instrument.equal in
  let get_data key = Result.map_l (fun bar -> get bar key) l in
  let* assoc_seq =
    List.fold_left
      (fun acc key ->
        let* acc = acc in
        let* data = get_data key in
        let items = List.flat_map Data.to_items data in
        let* combined = Data.of_items items in
        Result.return @@ Seq.cons (key, combined) acc)
      (Ok Seq.empty) keys
  in
  Result.return @@ Hashtbl.of_seq assoc_seq

let of_seq = Hashtbl.of_seq
let of_list l = of_seq @@ Seq.of_list l
