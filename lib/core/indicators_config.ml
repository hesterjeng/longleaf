type t = {
  fft : bool;
  compare_preloaded : bool;
  compute_live : bool;
  tacaml_indicators : Tacaml.t list;
}
[@@deriving show]

let default runtype =
  let compute_live = Runtype.real runtype in
  {
    fft = false;
    compare_preloaded = false;
    compute_live;
    tacaml_indicators = [];
  }

let make runtype tacaml_indicators =
  { (default runtype) with tacaml_indicators }
