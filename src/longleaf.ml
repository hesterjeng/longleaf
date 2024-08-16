module A = struct
  let x = 0
end

(* Define a module with a type and some functions *)
module MyMath = struct
  type operation = Add | Subtract | Multiply | Divide

  let apply_operation op x y =
    match op with
    | Add ->
        x + y
    | Subtract ->
        x - y
    | Multiply ->
        x * y
    | Divide ->
        if y <> 0 then x / y else failwith "Division by zero"

  let to_string op =
    match op with
    | Add ->
        "+"
    | Subtract ->
        "-"
    | Multiply ->
        "*"
    | Divide ->
        "/"
end

(* Use the module *)
let () =
  let x = 10 in
  let y = 5 in
  let ops = [MyMath.Add; MyMath.Subtract; MyMath.Multiply; MyMath.Divide] in
  List.iter
    (fun op ->
      let result = MyMath.apply_operation op x y in
      let op_str = MyMath.to_string op in
      Printf.printf "%d %s %d = %d\n" x op_str y result )
    ops ;
  (* More match statements outside the module *)
  let check_sign num =
    match num with
    | n when n > 0 ->
        "positive"
    | n when n < 0 ->
        "negative"
    | _ ->
        "zero"
  in
  let numbers = [x; -y; 0] in
  List.iter
    (fun n ->
      let sign = check_sign n in
      Printf.printf "The number %d is %s.\n" n sign )
    numbers
