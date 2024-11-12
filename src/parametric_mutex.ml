type 'a t = { mutable data : 'a; mutex : Eio.Mutex.t }

let make x = { data = x; mutex = Eio.Mutex.create () }

let set mut value =
  Eio.Mutex.use_rw ~protect:true mut.mutex @@ fun () -> mut.data <- value

let get mut = Eio.Mutex.use_ro mut.mutex @@ fun () -> mut.data
