type t = {
  fft : bool;
  compare_preloaded : bool;
  compute_live : bool;
  tacaml_indicators : Tacaml.t list;
}
[@@deriving show]

let default =
  {
    fft = false;
    compare_preloaded = false;
    compute_live = false;
    tacaml_indicators = [];
  }

let make tacaml_indicators = { default with tacaml_indicators }
