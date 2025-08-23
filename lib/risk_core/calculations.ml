open Base

type margin_type = 
  | Cash
  | RegT
  | Portfolio
  [@@deriving show]

type margin_requirements = {
  initial_margin: float;
  maintenance_margin: float;
  available_margin: float;
} [@@deriving show]

let calculate_margin_requirement ~margin_type ~order_value =
  match margin_type with
  | Cash -> { 
      initial_margin = order_value;
      maintenance_margin = order_value;
      available_margin = 0.0;
    }
  | RegT -> {
      initial_margin = order_value *. 0.5;
      maintenance_margin = order_value *. 0.25;
      available_margin = order_value *. 0.5;
    }
  | Portfolio -> {
      initial_margin = order_value *. 0.15;
      maintenance_margin = order_value *. 0.1;
      available_margin = order_value *. 0.85;
    }

let calculate_var ~positions ~confidence_level ~holding_period =
  (* Simplified VaR calculation *)
  let portfolio_value = List.fold positions ~init:0.0 ~f:(fun acc (_, value, _) ->
    acc +. value
  ) in
  let volatility = 0.02 in  (* 2% daily volatility assumption *)
  let z_score = match confidence_level with
    | 0.95 -> 1.645
    | 0.99 -> 2.326
    | _ -> 1.645
  in
  portfolio_value *. volatility *. z_score *. Float.sqrt holding_period

let calculate_stress_test ~positions ~scenario =
  (* Apply stress scenario to positions *)
  let stress_factor = match scenario with
    | `MarketCrash -> 0.8
    | `Volatility -> 1.2
    | `Liquidity -> 0.9
    | _ -> 1.0
  in
  List.map positions ~f:(fun (symbol, value, qty) ->
    (symbol, value *. stress_factor, qty)
  )

let calculate_exposure ~positions =
  let long_exposure, short_exposure = 
    List.fold positions ~init:(0.0, 0.0) ~f:(fun (long, short) (_, value, qty) ->
      if Float.(qty > 0.0) then
        (long +. value, short)
      else
        (long, short +. Float.abs value)
    )
  in
  {| 
    gross_exposure = long_exposure +. short_exposure;
    net_exposure = long_exposure -. short_exposure;
    long_exposure;
    short_exposure;
  |}

let calculate_buying_power ~cash_balance ~margin_type ~current_positions =
  let position_value = List.fold current_positions ~init:0.0 ~f:(fun acc (_, value, _) ->
    acc +. Float.abs value
  ) in
  match margin_type with
  | Cash -> cash_balance
  | RegT -> (cash_balance +. (position_value *. 0.5)) *. 2.0
  | Portfolio -> (cash_balance +. (position_value *. 0.85)) *. 6.67