type none
type some
type 'a t = { cutie : string; cute_score : float; cute_actions : string list }

let empty () : none t =
  { cutie = Obj.magic 0; cute_score = Obj.magic 0; cute_actions = Obj.magic 0 }

let make cutie cute_score cute_actions : some t =
  { cutie; cute_score; cute_actions }
