open Angstrom

let parens p = char '(' *> p <* char ')'

let quoted_string =
  char '\'' *> take_while (fun c -> not @@ Char.equal c '\'') <* char '\''

let parser =
  parens
  @@ lift2
       (fun s1 s2 -> (s1, s2))
       (quoted_string <* char ',' <* char ' ')
       quoted_string

let top (input : string) =
  match parse_string ~consume:Consume.All parser input with
  | Ok s -> s
  | Error e -> invalid_arg @@ Format.asprintf "%s: %s" e input
