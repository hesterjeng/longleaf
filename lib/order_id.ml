type t = Uuidm.t [@@deriving show]

let of_yojson = function
  | `String s -> (
    match Uuidm.of_string s with
    | Some uuid -> Ok uuid
    | None -> Error "Invalid UUID format")
  | _ -> Error "Expected UUID string"

let to_yojson uuid = `String (Uuidm.to_string uuid)
let of_string s = Uuidm.of_string s |> Option.get_exn_or "Invalid UUID"
let to_string uuid = Uuidm.to_string uuid
let generate () = Uuidm.v4_gen Util.random_state ()
let compare = Uuidm.compare
let equal = Uuidm.equal
