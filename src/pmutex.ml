type 'a t = {
  mutable data : 'a;
  mutex : (Eio.Mutex.t[@opaque]);
}
(* [@@deriving show, yojson] *)

let pp pp_data : 'a t Format.printer =
  fun fmt x ->
  Format.fprintf fmt "%a" pp_data x.data

let show pp_data x = Format.asprintf "%a" (pp pp_data) x

let make x = { data = x; mutex = Eio.Mutex.create () }

let set mut value =
  Eio.Mutex.use_rw ~protect:true mut.mutex @@ fun () -> mut.data <- value

let get mut = Eio.Mutex.use_ro mut.mutex @@ fun () -> mut.data
