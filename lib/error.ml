type t =
  [ `MissingData of string
  | `MissingClient of string
  | `UnsupportedOrder of string
  | (Piaf.Error.t[@printer Piaf.Error.pp_hum])
  | `JsonError of string
  | `Finished of string
  | `FatalError of string ]
[@@deriving show { with_path = false }]

let float_of_string (x : string) =
  match Float.of_string_opt x with
  | Some x -> Ok x
  | None ->
    Result.fail @@ `FatalError (Format.asprintf "Expected %s to be a float" x)

let int_of_string (x : string) =
  match Int.of_string x with
  | Some x -> Ok x
  | None ->
    Result.fail @@ `FatalError (Format.asprintf "Expected %s to be an int" x)

let fatal (x : string) = Result.fail @@ `FatalError x
let json (x : string) = Result.fail @@ `JsonError x
let missing_data (x : string) = Result.fail @@ `MissingData x

exception Longleaf_error

let raise (x : t) =
  Eio.traceln "@[%a@]@." pp x;
  raise Longleaf_error

let guard err f : ('a, t) result =
  try Result.return @@ f () with
  | _ -> err
