module type CLIENT = sig
  val longleaf_env : Environment.t
  val client : Piaf.Client.t
end
