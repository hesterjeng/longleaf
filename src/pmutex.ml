module Json = struct
  type 'a t = { data : 'a } [@@deriving show, yojson]
end

type 'a t = { mutable data : 'a; mutex : (Eio.Mutex.t[@opaque]) }
(* [@@deriving show, yojson] *)

let pp pp_data : 'a t Format.printer =
 fun fmt x -> Format.fprintf fmt "%a" pp_data x.data

let show pp_data x = Format.asprintf "%a" (pp pp_data) x
let make x = { data = x; mutex = Eio.Mutex.create () }

let set mut value =
  Eio.Mutex.use_rw ~protect:true mut.mutex @@ fun () -> mut.data <- value

let get mut = Eio.Mutex.use_ro mut.mutex @@ fun () -> mut.data
let of_json (x : 'a Json.t) = make x.data
let to_json (x : 'a t) : 'a Json.t = { data = x.data }
let t_of_yojson data_of_yojson x = Json.t_of_yojson data_of_yojson x |> of_json
let yojson_of_t data_to_yojson x = to_json x |> Json.yojson_of_t data_to_yojson
