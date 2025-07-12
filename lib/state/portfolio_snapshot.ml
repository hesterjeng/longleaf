type t = {
  time : Time.t;
  cash : float;
  portfolio_value : float;
  risk_free_value : float;
  orders : Order.t list;
}

let make time cash portfolio_value risk_free_value orders =
  { time; cash; portfolio_value; risk_free_value; orders }
