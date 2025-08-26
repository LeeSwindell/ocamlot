module Resp3 = Resp3

(* Optional: Re-export commonly used types/functions at top level *)
type resp_value = Resp3.resp_value =
  | SimpleString of string
  | SimpleError of string
  | Integer of int64
  | BulkString of string option
  | Array of resp_value list option
  | Null
  | Boolean of bool
  | Double of float
  | BigNumber of Z.t
  | BulkError of string
  | VerbatimString of {format: string; content: string}
  | Map of (resp_value * resp_value) list
  | Attribute of {attrs: (resp_value * resp_value) list; value: resp_value}
  | Set of resp_value list
  | Push of {kind: string; data: resp_value list}

(* Convenience function at top level *)
let parse_resp3 = Resp3.parse_string