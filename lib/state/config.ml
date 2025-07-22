type t = {
  placeholder : bool;
  indicator_config : Longleaf_core.Indicators_config.t;
}

let default =
  {
    placeholder = false;
    indicator_config = Longleaf_core.Indicators_config.default;
  }
