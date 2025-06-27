type t = { fft : bool; compare_preloaded : bool } [@@deriving show]

let default = { fft = false; compare_preloaded = false }
