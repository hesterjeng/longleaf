(** Longleaf Algorithmic Trading Platform *)

module Core = Longleaf_core
(** Core trading types and functionality *)

module Util = Longleaf_util
(** Utility functions and helpers *)

module Bars = Longleaf_bars
(** Market data and bar management *)

module Indicators = Longleaf_indicators
(** Technical indicators *)

module Apis = Longleaf_apis
(** External API integrations *)

module State = Longleaf_state
(** State management and configuration *)

module Backend = Longleaf_backend
(** Trading backends (live, paper, backtesting) *)

module Server = Longleaf_server
(** Web server and visualization *)
