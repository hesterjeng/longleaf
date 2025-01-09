type t = {
  shutdown_mutex : bool Pmutex.t;
  data_mutex : Bars.t Pmutex.t;
  indicators_mutex : Indicators.t Pmutex.t;
  orders_mutex : Order_history.t Pmutex.t;
  symbols_mutex : string option Pmutex.t;
  stats_mutex : Stats.t Pmutex.t;
}
