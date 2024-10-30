module type S = sig
  val tick : unit -> (unit, _) Lwt_result.t
end

module FiveMinuteLwt : S = struct
  let tick () = Lwt_unix.sleep 300.0 |> Lwt_result.ok
end

module ThirtyMinuteLwt : S = struct
  let tick () = Lwt_unix.sleep 1800.0 |> Lwt_result.ok
end

module ThirtySecondLwt : S = struct
  let tick () = Lwt_unix.sleep 30.0 |> Lwt_result.ok
end

module InstantLwt : S = struct
  let tick () = Lwt.return_unit |> Lwt_result.ok
end
