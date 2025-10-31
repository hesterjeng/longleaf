type t = {
  fft : bool;
  compare_preloaded : bool;
  compute_live : bool;
  compute_all_parallel : bool;
  tacaml_indicators : Tacaml.t list;
  print_tick_arg : bool;
}
[@@deriving show]

let default runtype =
  let compute_live = Runtype.real runtype in
  {
    fft = false;
    compare_preloaded = false;
    compute_live;
    tacaml_indicators = [];
    compute_all_parallel = true;
    print_tick_arg = false;
  }

let make runtype tacaml_indicators =
  { (default runtype) with tacaml_indicators }

let make_with_print_tick runtype tacaml_indicators print_tick_arg =
  { (default runtype) with tacaml_indicators; print_tick_arg }
