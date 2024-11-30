module type S = sig
  val tick : Eio_unix.Stdenv.base -> unit
  val length : float
end

module FiveMinute : S = struct
  let tick env = Eio.Time.sleep env#clock 300.0
  let length = 300.0
end

module ThirtyMinute : S = struct
  let tick env = Eio.Time.sleep env#clock 1800.0
  let length = 1800.0
end

module ThirtySecond : S = struct
  let tick env = Eio.Time.sleep env#clock 30.0
  let length = 30.0
end

module FiveSecond : S = struct
  let tick env = Eio.Time.sleep env#clock 5.0
  let length = 5.0
end

module OneSecond : S = struct
  let tick env = Eio.Time.sleep env#clock 1.0
  let length = 1.0
end

module Instant : S = struct
  let tick _ = ()
  let length = 0.0
end

module Make (Inp : sig
  val tick : float
end) : S = struct
  let tick env = Eio.Time.sleep env#clock Inp.tick
  let length = Inp.tick
end
