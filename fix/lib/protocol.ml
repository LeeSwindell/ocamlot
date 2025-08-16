open Base

type fix_message_type = 
  | NewOrderSingle
  | ExecutionReport
  | OrderCancelRequest
  | OrderCancelReject
  [@@deriving show]

type fix_field = string * string [@@deriving show]

type fix_message = {
  msg_type: fix_message_type;
  fields: fix_field list;
} [@@deriving show]

let fix_delimiter = "\001"

let encode_field (tag, value) =
  tag ^ "=" ^ value ^ fix_delimiter

let encode_message msg =
  let type_tag = match msg.msg_type with
    | NewOrderSingle -> "35=D"
    | ExecutionReport -> "35=8"
    | OrderCancelRequest -> "35=F"
    | OrderCancelReject -> "35=9"
  in
  let fields_str = List.fold msg.fields ~init:"" ~f:(fun acc field ->
    acc ^ encode_field field) in
  type_tag ^ fix_delimiter ^ fields_str

let decode_message _raw_msg =
  failwith "FIX decoding not implemented yet"