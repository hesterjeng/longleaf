module Hashtbl_make = Hashtbl.Make
module Hashtbl = Hashtbl_make (Instrument)
module Data = Data

let fold (x : 'a Hashtbl.t) init f = Hashtbl.fold f x init

module Latest = Latest

type t = Data.t Hashtbl.t

let copy x =
  let copied = Hashtbl.copy x in
  Hashtbl.iter (fun s ph -> Hashtbl.replace copied s @@ Data.copy ph) x;
  copied

(* Helper function to deserialize a single symbol *)
let deserialize_symbol (symbol_string, symbol_json) =
  let ( let* ) = Result.( let* ) in
  let* instrument = Instrument.of_string_res symbol_string in
  let* ph = Data.t_of_yojson symbol_json in
  Result.return @@ (instrument, ph)

let t_of_yojson ?eio_env (json : Yojson.Safe.t) : (t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  let bars = Yojson.Safe.Util.member "bars" json in
  let assoc = Yojson.Safe.Util.to_assoc bars in

  let* mapped =
    match eio_env with
    | Some env ->
      (* Parallel deserialization using Work_pool *)
      let results =
        Util.Work_pool.Work_pool.parallel_map_result ~eio_env:env
          ~log_performance:true ~f:deserialize_symbol assoc
      in

      (* Convert Work_pool results to Result.map_l format *)
      List.fold_left
        (fun acc result ->
          match (acc, result) with
          | Ok acc_list, Ok (Ok symbol_data) -> Ok (symbol_data :: acc_list)
          | Ok _, Ok (Error e) -> Error e
          | Error e, _ -> Error e
          | _, Error exn -> Error (`FatalError (Printexc.to_string exn)))
        (Ok []) results
      |> Result.map List.rev
    | None ->
      (* Sequential deserialization (original behavior) *)
      Result.map_l deserialize_symbol assoc
  in

  let seq = Seq.of_list mapped in
  Result.return @@ Hashtbl.of_seq seq

let yojson_of_t (x : t) : (Yojson.Safe.t, Error.t) result =
  let ( let* ) = Result.( let* ) in
  let* res =
    Error.guard (Error.json "Error in Bars.yojson_of_t") @@ fun () ->
    fold x [] @@ fun symbol data acc ->
    let json = Data.yojson_of_t data in
    (Instrument.symbol symbol, json) :: acc
  in
  let core : Yojson.Safe.t = `Assoc res in
  Result.return @@ `Assoc [ ("bars", core) ]

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
  | _ -> Error.fatal "Mismatched price history v2 matrices in Bars v2"

let timestamp (x : t) =
  let res =
    fold x None @@ fun _symbol data acc ->
    let timestamp = Data.get_top data Time in
    match acc with
    | None -> Some timestamp
    | Some prev ->
      assert (Float.equal prev timestamp);
      Some prev
  in
  match res with
  | None -> Error.fatal "Could not get Bars.timestamp"
  | Some t -> (
    match Ptime.of_float_s t with
    | Some s -> Result.return s
    | None -> Error.fatal "Error converting timestamp in Bars.timestamp")
(* Ptime.of_float_s res *)

let of_file ?eio_env file =
  let start_time = Unix.gettimeofday () in
  Eio.traceln "Starting JSON parsing from %s" file;
  let json_result = Yojson.Safe.from_file file in
  let json_time = Unix.gettimeofday () in
  Eio.traceln "JSON file read took %.3fs" (json_time -. start_time);

  Eio.traceln "Starting bars deserialization";
  let result =
    t_of_yojson ?eio_env json_result |> function
    | Ok x ->
      let end_time = Unix.gettimeofday () in
      Eio.traceln "Bars deserialization took %.3fs" (end_time -. json_time);
      Eio.traceln "Total bars loading took %.3fs" (end_time -. start_time);
      Eio.traceln "Finished loading bars from %s" file;
      x
    | Error e -> invalid_arg @@ Error.show e
  in
  result

(* let append (latest : Latest.t) (x : t) : (unit, Error.t) result = *)
(*   let ( let* ) = Result.( let* ) in *)
(*   fold latest (Ok ()) @@ fun symbol item acc -> *)
(*   let* acc = acc in *)
(*   let* ph = get x symbol in *)
(*   let* appended = Data.add_column ph item in *)
(*   Hashtbl.replace x symbol appended; *)
(*   Result.return acc *)

let pp : t Format.printer =
 fun fmt _ -> Format.fprintf fmt "@[<Bars.pp opaque>@]@."

let pp_stats : t Format.printer =
 fun fmt b ->
  match length b with
  | Ok len -> Format.fprintf fmt "@[bars length: %d@]@." len
  | Error _ -> Format.fprintf fmt "@[Error when printing bars stats@]@."

(* let latest_i (x : t) i = *)
(*   let ( let* ) = Result.( let* ) in *)
(*   let res = Latest.empty () in *)
(*   let* () = *)
(*     fold x (Ok ()) @@ fun instrument ph acc -> *)
(*     let* _ = acc in *)
(*     let get = Data.get ph in *)
(*     let* timestamp = *)
(*       get Time i |> Ptime.of_float_s |> function *)
(*       | Some x -> Ok x *)
(*       | None -> Error.fatal "Illegal time in Bars.last_bar" *)
(*     in *)
(*     Latest.set res instrument *)
(*     @@ { *)
(*          timestamp; *)
(*          last = get Last i; *)
(*          open_ = get Open i; *)
(*          high = get High i; *)
(*          low = get Low i; *)
(*          close = get Close i; *)
(*          volume = Int.of_float @@ get Volume i; *)
(*        }; *)
(*     Result.return () *)
(*   in *)
(*   Result.return res *)

(* let to_queue (x : t) : (Latest.t Queue.t, Error.t) result = *)
(*   let ( let* ) = Result.( let* ) in *)
(*   let q : Latest.t Queue.t = Queue.create () in *)
(*   let* len = length x in *)
(*   let* () = *)
(*     let r = Int.range' 0 len in *)
(*     Iter.fold *)
(*       (fun acc i -> *)
(*         let* _ = acc in *)
(*         let* latest = latest_i x i in *)
(*         Queue.add latest q; *)
(*         Result.return ()) *)
(*       (Ok ()) r *)
(*   in *)
(*   Result.return q *)

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

let set_current bars i =
  (fun f -> Hashtbl.filter_map_inplace f bars) @@ fun _ data ->
  Option.return @@ { data with Data.current = i }

let get_current (bars : t) =
  let ( let* ) = Result.( let* ) in
  fold bars (Ok 0) @@ fun _ data acc ->
  let* acc = acc in
  match acc with
  | 0 -> Ok data.Data.current
  | n when n > 0 -> (
    match n = data.Data.current with
    | true -> Ok n
    | false -> Error.fatal "Current mismatch in bars.ml")
  | _ -> Error.fatal "Bad bars length in bars.ml"

let of_seq = Hashtbl.of_seq
let of_list l = of_seq @@ Seq.of_list l

let print_to_file_direct bars filename =
  let ( let* ) = Result.( let* ) in
  let* bars_json = yojson_of_t bars in
  let str = Yojson.Safe.to_string bars_json in
  let oc = open_out filename in
  output_string oc str;
  close_out oc;
  Result.return ()

let print_to_file ?(filename : string option) bars prefix =
  let tail =
    match filename with
    | Some n -> n
    | None -> Util.random_filename ()
  in
  let filename = Format.sprintf "data/%s_%s.json" prefix tail in
  print_to_file_direct bars filename
