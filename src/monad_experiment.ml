[@@@warning "-27"]

module Cmdliner = Cmdliner

module Att1 = struct
  type signal = Buy | Sell | Fail [@@deriving eq]
  type 'a t = 'a -> signal

  let bind (f : 'a -> 'a t) (x : 'a t) : 'a t =
   fun a ->
    let f1 = f a in
    let f2 = f1 a in
    let f3 = x a in
    if equal_signal f2 f3 then f2 else Fail

  let ( let* ) x f = bind f x

  let t1 : 'a t =
   fun _ ->
    Eio.traceln "t1";
    Fail

  let t2 : 'a t =
   fun _ ->
    Eio.traceln "t2";
    Fail

  let boom (input : 'a) =
    let* a = t1 in
    let* h = t2 in
    t2
end

module Att2 = struct
  open CCFun.Monad (Int)

  let f : float t = fun x -> Float.of_int x
  let g : float t = fun x -> Float.of_int x -. 1.0

  let h (x : string) : float t =
   fun (inp : int) -> Float.of_int inp +. Float.of_string_exn x

  let bind f x = x >>= f
  let ( let* ) f x = bind f x

  let h =
    let* x = h in
    "woah"
end
