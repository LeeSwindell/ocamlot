open Base

type instrument_id = string [@@deriving show]
type order_id = string [@@deriving show]
type client_id = string [@@deriving show]

type side = Buy | Sell [@@deriving show]

type order_type = 
  | Market
  | Limit of float
  | Stop of float
  | StopLimit of float * float
  [@@deriving show]

type currency = USD | EUR | GBP | JPY [@@deriving show]

type decimal = float [@@deriving show]

type timestamp = float [@@deriving show]