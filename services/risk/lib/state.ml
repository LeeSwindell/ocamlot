open Base
open Lwt.Syntax
open Ocamlot_core_types
open Ocamlot_risk_core

type risk_state = {
  portfolio_states: (string, Checks.portfolio_state) Hashtbl.t;
  limits: (string, Checks.risk_limits) Hashtbl.t;
  market_prices: (string, float) Hashtbl.t;
  mutable checks_performed: int;
  mutable checks_failed: int;
}

let create () = {
  portfolio_states = Hashtbl.create (module String);
  limits = Hashtbl.create (module String);
  market_prices = Hashtbl.create (module String);
  checks_performed = 0;
  checks_failed = 0;
}

let get_portfolio_state state account_id =
  match Hashtbl.find state.portfolio_states account_id with
  | Some portfolio -> portfolio
  | None ->
    let new_portfolio = Checks.{
      positions = Hashtbl.create (module String);
      cash_balance = 100000.0;  (* Default starting balance *)
      daily_volume = 0.0;
      total_exposure = 0.0;
    } in
    Hashtbl.set state.portfolio_states ~key:account_id ~data:new_portfolio;
    new_portfolio

let get_limits state account_id =
  match Hashtbl.find state.limits account_id with
  | Some limits -> limits
  | None ->
    let default = Checks.default_limits in
    Hashtbl.set state.limits ~key:account_id ~data:default;
    default

let update_market_price state instrument_id price =
  Hashtbl.set state.market_prices ~key:instrument_id ~data:price

let get_market_price state instrument_id =
  Hashtbl.find state.market_prices instrument_id
  |> Option.value ~default:100.0  (* Default price if not found *)

let update_position state account_id ~instrument_id ~quantity ~side ~price =
  let portfolio = get_portfolio_state state account_id in
  let current_pos = Hashtbl.find portfolio.positions instrument_id in
  let new_pos = match current_pos, side with
    | None, Types.Buy ->
      Checks.{ 
        instrument_id;
        quantity;
        avg_price = price;
        market_value = quantity *. price;
        unrealized_pnl = 0.0;
      }
    | None, Types.Sell ->
      Checks.{
        instrument_id;
        quantity = Float.neg quantity;
        avg_price = price;
        market_value = Float.neg (quantity *. price);
        unrealized_pnl = 0.0;
      }
    | Some pos, Types.Buy ->
      let new_qty = pos.quantity +. quantity in
      let new_avg = ((pos.quantity *. pos.avg_price) +. (quantity *. price)) /. new_qty in
      Checks.{
        pos with
        quantity = new_qty;
        avg_price = new_avg;
        market_value = new_qty *. price;
      }
    | Some pos, Types.Sell ->
      let new_qty = pos.quantity -. quantity in
      Checks.{
        pos with
        quantity = new_qty;
        market_value = new_qty *. price;
      }
  in
  Hashtbl.set portfolio.positions ~key:instrument_id ~data:new_pos

let record_check_result state ~passed =
  state.checks_performed <- state.checks_performed + 1;
  if not passed then
    state.checks_failed <- state.checks_failed + 1

let get_statistics state =
  let pass_rate = 
    if state.checks_performed > 0 then
      Float.of_int (state.checks_performed - state.checks_failed) /. 
      Float.of_int state.checks_performed *. 100.0
    else 100.0
  in
  `Assoc [
    ("checks_performed", `Int state.checks_performed);
    ("checks_failed", `Int state.checks_failed);
    ("pass_rate", `Float pass_rate);
  ]