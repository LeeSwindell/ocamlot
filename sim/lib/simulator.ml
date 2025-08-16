open Base
open Lwt.Syntax
open Ocamlot_common

type sim_config = {
  instruments: Types.instrument_id list;
  initial_prices: (Types.instrument_id * Types.decimal) list;
  volatility: float;
  tick_interval_ms: int;
} [@@deriving show]

type simulator = {
  config: sim_config;
  current_prices: (Types.instrument_id, Types.decimal) Hashtbl.t;
  is_running: bool ref;
}

let create_simulator config =
  let current_prices = Hashtbl.create (module String) in
  List.iter config.initial_prices ~f:(fun (instrument_id, price) ->
    Hashtbl.set current_prices ~key:instrument_id ~data:price
  );
  {
    config;
    current_prices;
    is_running = ref false;
  }

let generate_price_movement ~current_price ~volatility =
  let random_factor = (Random.float 2.0) -. 1.0 in
  let change = current_price *. volatility *. random_factor *. 0.01 in
  Float.max 0.01 (current_price +. change)

let run_simulation sim ~on_price_update =
  sim.is_running := true;
  let rec loop () =
    if !(sim.is_running) then
      let* () = Lwt_unix.sleep (Float.of_int sim.config.tick_interval_ms /. 1000.0) in
      List.iter sim.config.instruments ~f:(fun instrument_id ->
        match Hashtbl.find sim.current_prices instrument_id with
        | Some current_price ->
          let new_price = generate_price_movement ~current_price ~volatility:sim.config.volatility in
          Hashtbl.set sim.current_prices ~key:instrument_id ~data:new_price;
          on_price_update instrument_id new_price
        | None -> ()
      );
      loop ()
    else
      Lwt.return ()
  in
  loop ()

let stop_simulation sim =
  sim.is_running := false