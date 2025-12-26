[@@@warning "-32"]

let some_symbols =
  [
    "NVDA";
    "TSLA";
    "AAPL";
    "MSFT";
    "NFLX";
    "META";
    "AMZN";
    "AMD";
    "AVGO";
    "ELV";
    "UNH";
    "MU";
    "V";
    "GOOG";
    "SMCI";
    "MSTR";
    "UBER";
    "LLY";
    "SPY";
  ]

let oex = [ "OEX" ]
let spy = [ "SPY" ]

(** Major liquid ETFs - high volume, tight spreads, ideal for HFT-style strategies *)
let etfs = [
  "SPY";   (* S&P 500 *)
  "QQQ";   (* Nasdaq 100 *)
  "IWM";   (* Russell 2000 *)
  "DIA";   (* Dow 30 *)
  "GLD";   (* Gold *)
  "TLT";   (* 20+ Year Treasury *)
  "XLF";   (* Financials *)
  "XLE";   (* Energy *)
  "XLK";   (* Tech *)
  "XLV";   (* Healthcare *)
  "EEM";   (* Emerging Markets *)
  "HYG";   (* High Yield Corporate *)
  "LQD";   (* Investment Grade Corporate *)
]

let sp100 =
  [
    "PLTR";
    "SPY";
    "AAPL";
    "ABBV";
    "ABT";
    "ACN";
    "ADBE";
    "AIG";
    "AMD";
    "AMGN";
    "AMT";
    "AMZN";
    (* "AVGO"; *)
    "AXP";
    "BA";
    "BAC";
    "BK";
    "BKNG";
    "BLK";
    "BMY";
    (* "BRK.B"; *)
    "C";
    "CAT";
    "CHTR";
    "CL";
    "CMCSA";
    "COF";
    "COP";
    "COST";
    "CRM";
    "CSCO";
    "CVS";
    "CVX";
    "DE";
    "DHR";
    "DIS";
    (* "DOW"; *)
    "DUK";
    "EMR";
    (* "F"; *)
    "FDX";
    "GD";
    "GE";
    "GILD";
    "GM";
    (* "GOOG"; *)
    "GOOGL";
    "GS";
    "HD";
    "HON";
    "IBM";
    "INTC";
    "INTU";
    "JNJ";
    "JPM";
    "KHC";
    "KO";
    (* "LIN"; *)
    "LLY";
    "LMT";
    "LOW";
    "MA";
    "MCD";
    "MDLZ";
    "MDT";
    "MET";
    "META";
    "MMM";
    "MO";
    "MRK";
    "MS";
    "MSFT";
    "NEE";
    "NFLX";
    "NKE";
    "NVDA";
    "ORCL";
    "PEP";
    "PFE";
    "PG";
    "PM";
    "PYPL";
    "QCOM";
    "RTX";
    "SBUX";
    "SCHW";
    "SO";
    "SPG";
    "T";
    "TGT";
    "TMO";
    "TMUS";
    "TSLA";
    "TXN";
    "UNH";
    "UNP";
    "UPS";
    "USB";
    "V";
    "VZ";
    "WFC";
    "WMT";
    "XOM";
  ]

let sp100_spy = "SPY" :: sp100

(** Sector-based collections for more targeted strategies *)

(** Utilities - low volatility, range-bound, good for mean reversion *)
let utilities = [ "DUK"; "NEE"; "SO" ]

(** Consumer staples - defensive, stable, good for mean reversion *)
let staples = [ "PG"; "KO"; "PEP"; "WMT"; "COST"; "CL"; "PM"; "MO"; "MDLZ"; "KHC" ]

(** Defensive stocks - utilities + staples combined *)
let defensive = utilities @ staples

(** Healthcare - mixed behavior, some defensive characteristics *)
let healthcare = [ "UNH"; "JNJ"; "PFE"; "LLY"; "ABBV"; "MRK"; "AMGN"; "BMY"; "GILD"; "CVS"; "MDT"; "DHR"; "TMO" ]

(** Financials - rate-sensitive, macro-driven *)
let financials = [ "JPM"; "BAC"; "GS"; "MS"; "C"; "WFC"; "BK"; "COF"; "SCHW"; "AXP"; "BLK"; "USB"; "MET" ]

(** Tech/Growth - momentum-driven, don't mean-revert well in bull markets *)
let tech_growth = [ "AAPL"; "MSFT"; "NVDA"; "AMZN"; "GOOGL"; "META"; "NFLX"; "TSLA"; "AMD"; "CRM"; "ADBE"; "INTU"; "ORCL"; "CSCO"; "INTC"; "QCOM"; "TXN" ]

(** Tech + SPY *)
let tespy = "SPY" :: tech_growth

(** Energy - commodity-driven *)
let energy = [ "XOM"; "CVX"; "COP" ]

(** Industrials - cyclical *)
let industrials = [ "CAT"; "DE"; "GE"; "HON"; "BA"; "LMT"; "RTX"; "UNP"; "UPS"; "FDX"; "EMR"; "GD"; "MMM" ]

(** Consumer discretionary - mixed *)
let consumer_discretionary = [ "HD"; "LOW"; "MCD"; "SBUX"; "NKE"; "TGT"; "BKNG" ]

(** Telecom *)
let telecom = [ "T"; "VZ"; "TMUS"; "CHTR"; "CMCSA" ]

(** Real estate *)
let real_estate = [ "AMT"; "SPG" ]

(** Non-tech SP100 - everything except momentum tech *)
let sp100_no_tech =
  List.filter (fun s -> not (List.mem s tech_growth)) sp100

(** Low volatility proxy - defensive + healthcare *)
let low_vol = defensive @ healthcare

(** Collection name to symbol list mapping *)
let all_collections : (string * string list) list =
  [
    ("sp100", sp100);
    ("sp100_spy", sp100_spy);
    ("defensive", defensive);
    ("utilities", utilities);
    ("staples", staples);
    ("healthcare", healthcare);
    ("financials", financials);
    ("tech", tech_growth);
    ("tespy", tespy);
    ("energy", energy);
    ("industrials", industrials);
    ("consumer", consumer_discretionary);
    ("telecom", telecom);
    ("real_estate", real_estate);
    ("sp100_no_tech", sp100_no_tech);
    ("low_vol", low_vol);
    ("spy", spy);
    ("etfs", etfs);
  ]

(** Get collection by name, returns None if not found *)
let get name =
  List.assoc_opt ~eq:String.equal name all_collections

(** Get collection by name, raises if not found *)
let get_exn name =
  match get name with
  | Some c -> c
  | None -> failwith (Printf.sprintf "Unknown ticker collection: %s" name)

(** List all available collection names *)
let available () =
  List.map fst all_collections
