open Base
open Lwt.Syntax
open Ocamlot_core_types
open Ocamlot_core_domain
open Ocamlot_risk_core
open Ocamlot_infrastructure_nats

let handle_risk_check_request state nats_client request =
  let order_id = request##order_id in
  let account_id = request##account_id in
  let instrument_id = request##instrument in
  let quantity = request##quantity in
  let side = match request##side with
    | "buy" -> Types.Buy
    | _ -> Types.Sell
  in
  
  (* Create a temporary order for checking *)
  let order = Order.create_order
    ~id:order_id
    ~client_id:account_id
    ~instrument_id
    ~side
    ~order_type:(Types.Market)  (* Assume market for risk check *)
    ~quantity
    ~timestamp:(Unix.time ())
  in
  
  let portfolio = State.get_portfolio_state state account_id in
  let limits = State.get_limits state account_id in
  let market_price = State.get_market_price state instrument_id in
  
  let result = Checks.run_pre_trade_checks ~limits ~portfolio ~order ~market_price in
  
  let passed = match result with
    | Checks.Passed -> true
    | Checks.Warning _ -> true  (* Allow with warning *)
    | Checks.Failed _ -> false
  in
  
  State.record_check_result state ~passed;
  
  let response_json = Yojson.Safe.to_string (`Assoc [
    ("order_id", `String order_id);
    ("passed", `Bool passed);
    ("result", `String (Checks.show_risk_result result));
    ("timestamp", `Float (Unix.time ()));
  ]) in
  
  let* () = Nats_client.publish nats_client
    ~subject:(Printf.sprintf "risk.check_response.%s" order_id)
    ~payload:response_json
  in
  
  (* Also publish to general risk events stream *)
  let* () = Nats_client.publish nats_client
    ~subject:"risk.events"
    ~payload:response_json
  in
  
  Lwt.return (Ok result)

let handle_position_update state request =
  let account_id = request##account_id in
  let instrument_id = request##instrument_id in
  let quantity = request##quantity in
  let side = match request##side with
    | "buy" -> Types.Buy
    | _ -> Types.Sell
  in
  let price = request##price in
  
  State.update_position state account_id ~instrument_id ~quantity ~side ~price;
  Lwt.return_unit

let handle_market_data_update state update =
  let instrument_id = update##instrument_id in
  let price = (update##bid +. update##ask) /. 2.0 in
  State.update_market_price state instrument_id price;
  Lwt.return_unit

let handle_limits_update state nats_client update =
  let account_id = update##account_id in
  let new_limits = Checks.{
    max_order_size = update##max_order_size;
    max_daily_volume = update##max_daily_volume;
    max_position_size = update##max_position_size;
    max_notional_exposure = update##max_notional_exposure;
    concentration_limit = update##concentration_limit;
  } in
  
  Hashtbl.set state.limits ~key:account_id ~data:new_limits;
  
  let response_json = Yojson.Safe.to_string (`Assoc [
    ("account_id", `String account_id);
    ("status", `String "limits_updated");
    ("timestamp", `Float (Unix.time ()));
  ]) in
  
  let* () = Nats_client.publish nats_client
    ~subject:"risk.limits.updated"
    ~payload:response_json
  in
  
  Lwt.return_unit