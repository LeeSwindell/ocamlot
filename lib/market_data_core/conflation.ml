open Base
open Ocamlot_core_types

type quote = {
  instrument_id: string;
  bid: float;
  bid_size: float;
  ask: float;
  ask_size: float;
  timestamp: Types.timestamp;
} [@@deriving show]

type conflation_config = {
  window_ms: int;
  max_updates_per_window: int;
  always_send_on_cross: bool;
}

type conflated_quote = {
  original: quote;
  updates_count: int;
  window_start: Types.timestamp;
}

let default_config = {
  window_ms = 100;
  max_updates_per_window = 10;
  always_send_on_cross = true;
}

let is_crossed quote =
  Float.(quote.bid >= quote.ask)

let should_send_update config current_state new_quote =
  match current_state with
  | None -> true
  | Some state ->
    let time_elapsed = new_quote.timestamp -. state.window_start in
    let window_expired = Float.(time_elapsed >= Float.of_int config.window_ms /. 1000.0) in
    let max_updates_reached = state.updates_count >= config.max_updates_per_window in
    let is_crossed_quote = config.always_send_on_cross && is_crossed new_quote in
    
    window_expired || max_updates_reached || is_crossed_quote

let create_conflated_state quote =
  {
    original = quote;
    updates_count = 1;
    window_start = quote.timestamp;
  }

let update_conflated_state state quote =
  if Float.(quote.timestamp -. state.window_start >= 0.1) then
    create_conflated_state quote
  else
    { state with 
      original = quote;
      updates_count = state.updates_count + 1;
    }

let conflate_quotes quotes config =
  List.fold quotes ~init:([], None) ~f:(fun (output, state) quote ->
    if should_send_update config state quote then
      let new_state = create_conflated_state quote in
      (quote :: output, Some new_state)
    else
      match state with
      | Some s -> (output, Some (update_conflated_state s quote))
      | None -> (output, Some (create_conflated_state quote))
  )
  |> fst
  |> List.rev