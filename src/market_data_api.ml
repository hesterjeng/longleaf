open Lwt
open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix
module Log = (val Logs.src_log Logs.(Src.create "data-api"))
open Trading_types

let h = Trading_api.h
