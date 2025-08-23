open Base

type error = 
  | Invalid_order of string
  | Risk_violation of string
  | Market_data_error of string
  | System_error of string
  [@@deriving show]

let error_to_string = function
  | Invalid_order msg -> "Invalid order: " ^ msg
  | Risk_violation msg -> "Risk violation: " ^ msg
  | Market_data_error msg -> "Market data error: " ^ msg
  | System_error msg -> "System error: " ^ msg