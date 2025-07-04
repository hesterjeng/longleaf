type t = { fft : bool; compare_preloaded : bool; compute_live : bool }
[@@deriving show]

let default = { fft = false; compare_preloaded = false; compute_live = false }
