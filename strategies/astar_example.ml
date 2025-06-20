open Astar_search
open EnumeratedSignal

let buy = Or (AtomSet.of_list [ Atom.RSI_lt Thirty ])
let sell = Or (AtomSet.of_list [ Atom.FSO_k_gt Ten ])

module Make = (val to_strategy buy sell)
