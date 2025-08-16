open Base
open Lwt.Syntax
open Ocamlot_common

type market_data = {
  instrument_id: Types.instrument_id;
  bid: Types.decimal;
  ask: Types.decimal;
  last_price: Types.decimal;
  volume: Types.decimal;
  timestamp: Types.timestamp;
} [@@deriving show]

type feed_config = {
  instruments: Types.instrument_id list;
  update_interval_ms: int;
} [@@deriving show]

let create_feed_config ~instruments ~update_interval_ms =
  { instruments; update_interval_ms }

let start_feed ~config ~callback =
  let rec loop () =
    let* () = Lwt_unix.sleep (Float.of_int config.update_interval_ms /. 1000.0) in
    List.iter config.instruments ~f:(fun instrument_id ->
      let mock_data = {
        instrument_id;
        bid = Random.float 100.0 +. 50.0;
        ask = Random.float 100.0 +. 50.5;
        last_price = Random.float 100.0 +. 50.25;
        volume = Random.float 10000.0;
        timestamp = Unix.time ();
      } in
      callback mock_data
    );
    loop ()
  in
  loop ()