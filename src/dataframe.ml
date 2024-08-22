module Log = (val Logs.src_log Logs.(Src.create "dataframe"))

type t = Owl.Dataframe.t

module Element = struct

  type ticker =
    {
      index : string;
      price : float;
    }

  type t =
    {
      date : int;
      information : ticker list;
    }

  let of_json (x : Yojson.Safe.t) =
    match x with
    | `Assoc l ->
      let info_type, amt = List.hd l in
      Log.app (fun k -> k "%a" Format.(pair String.pp Yojson.Safe.pp) x);
      invalid_arg "WIP"
    | _ -> invalid_arg "Not OK"

end

let of_json (x : Yojson.Safe.t) =
  match x with
  | `List l ->
    List.map Element.of_json l
  | _ -> invalid_arg "Dataframe.of_json"
