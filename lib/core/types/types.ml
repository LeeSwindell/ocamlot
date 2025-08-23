open Base

type instrument_id = string [@@deriving show, yojson]
type order_id = string [@@deriving show, yojson]
type client_id = string [@@deriving show, yojson]

type side = Buy | Sell [@@deriving show, yojson]

type order_type = 
  | Market
  | Limit of float
  | Stop of float
  | StopLimit of float * float
  [@@deriving show, yojson]

type currency = USD | EUR | GBP | JPY [@@deriving show, yojson]

type decimal = float [@@deriving show, yojson]

type timestamp = float [@@deriving show, yojson]