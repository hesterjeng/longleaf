type t = {
  fft : bool;
  compare_preloaded : bool;
  compute_live : bool;
  custom_indicators : Tacaml.t list;
}
[@@deriving show]

let default =
  {
    fft = false;
    compare_preloaded = false;
    compute_live = false;
    custom_indicators = [];
  }

let with_custom_indicators custom_indicators config =
  { config with custom_indicators }

let add_custom_indicator indicator config =
  { config with custom_indicators = indicator :: config.custom_indicators }
