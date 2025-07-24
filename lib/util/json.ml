type t = Yojson.Safe.t

let ( let* ) = Result.( let* )

let member x y =
  try Result.return @@ Yojson.Safe.Util.member x y with
  | _ -> Error "Json problem"

let float (x : t) =
  match x with
  | `Float f -> Result.return f
  | _ -> Error "Expected float"

let bool (x : t) =
  match x with
  | `Bool f -> Result.return f
  | _ -> Error "Expected float"

let int (x : t) =
  match x with
  | `Int f -> Result.return f
  | _ -> Error "Expected float"

let float_of_string (x : t) =
  match x with
  | `String f -> (
    Float.of_string_opt f |> function
    | Some x -> Ok x
    | None -> Error "Unable to convert float to string")
  | _ -> Error "Expected float"

let string (x : t) =
  match x with
  | `String f -> Result.return f
  | _ -> Error "Expected string"

let float_member m (x : t) =
  let* f = member m x in
  float f

let string_member m (x : t) =
  let* f = member m x in
  string f

let float_of_string_member m (x : t) =
  let* f = member m x in
  float_of_string f

let int_member m (x : t) =
  let* f = member m x in
  int f

let bool_member m (x : t) =
  let* f = member m x in
  bool f
