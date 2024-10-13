module type S = sig
  val tick : unit -> (unit, _) Lwt_result.t
end

module FiveMinute : S = struct
  let tick () = Lwt_unix.sleep 300.0 |> Lwt_result.ok
end

module ThirtyMinute : S = struct
  let tick () = Lwt_unix.sleep 1800.0 |> Lwt_result.ok
end

module Instant : S = struct
  let tick () = Lwt.return_unit |> Lwt_result.ok
end
