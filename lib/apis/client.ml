module type CLIENT = sig
  val longleaf_env : Environment.t
  val client : Cohttp_eio.Client.t
end
