open! Core
module Price = Exchange_core.Price

let schema_id = 8
let schema_version = 5

module Side = struct
  type t =
    | Buy
    | Sell
  [@@deriving sexp, compare, equal, enumerate]

  let to_int = function
    | Buy -> 1
    | Sell -> 2
  ;;

  let of_int = function
    | 1 -> Some Buy
    | 2 -> Some Sell
    | _ -> None
  ;;
end

module Ord_type = struct
  type t =
    | Market_with_protection
    | Limit
    | Stop_with_protection
    | Stop_limit
    | Market_with_leftover_as_limit
  [@@deriving sexp, compare, equal, enumerate]

  let to_char = function
    | Market_with_protection -> '1'
    | Limit -> '2'
    | Stop_with_protection -> '3'
    | Stop_limit -> '4'
    | Market_with_leftover_as_limit -> 'K'
  ;;

  let of_char c = List.find all ~f:(fun t -> Char.equal (to_char t) c)
end

module Time_in_force = struct
  type t =
    | Day
    | Gtc
    | Fak
    | Fok
    | Gtd
  [@@deriving sexp, compare, equal, enumerate]

  let to_int = function
    | Day -> 0
    | Gtc -> 1
    | Fak -> 3
    | Fok -> 4
    | Gtd -> 6
  ;;

  let of_int i = List.find all ~f:(fun t -> to_int t = i)
end

(* Decoding failures inside a codec raise [Decode_error]; [decode] turns them into errors. *)
exception Decode_error of string

let fail fmt = ksprintf (fun s -> raise (Decode_error s)) fmt

let decode_side b ~pos =
  let v = Sbe.get_u8 b ~pos in
  match Side.of_int v with
  | Some s -> s
  | None -> fail "invalid Side %d" v
;;

let decode_ord_type b ~pos =
  let c = Sbe.get_char b ~pos in
  match Ord_type.of_char c with
  | Some t -> t
  | None -> fail "invalid OrdType %C" c
;;

let decode_tif b ~pos =
  let v = Sbe.get_u8 b ~pos in
  match Time_in_force.of_int v with
  | Some t -> t
  | None -> fail "invalid TimeInForce %d" v
;;

(* FaultToleranceIndicator, SplitMsg and similar optional enums this codec does not model. *)
let set_null_u8 b ~pos = Sbe.set_u8_opt b ~pos None

module Negotiate = struct
  type t =
    { hmac_signature : string
    ; access_key_id : string
    ; uuid : int
    ; request_timestamp : int
    ; session : string
    ; firm : string
    }
  [@@deriving sexp, compare, equal]

  let template_id = 500
  let block_length = 76

  let write b ~base t =
    Sbe.set_string b ~pos:base ~len:32 t.hmac_signature;
    Sbe.set_string b ~pos:(base + 32) ~len:20 t.access_key_id;
    Sbe.set_u64 b ~pos:(base + 52) t.uuid;
    Sbe.set_u64 b ~pos:(base + 60) t.request_timestamp;
    Sbe.set_string b ~pos:(base + 68) ~len:3 t.session;
    Sbe.set_string b ~pos:(base + 71) ~len:5 t.firm
  ;;

  let read b ~base =
    { hmac_signature = Sbe.get_string b ~pos:base ~len:32
    ; access_key_id = Sbe.get_string b ~pos:(base + 32) ~len:20
    ; uuid = Sbe.get_u64 b ~pos:(base + 52)
    ; request_timestamp = Sbe.get_u64 b ~pos:(base + 60)
    ; session = Sbe.get_string b ~pos:(base + 68) ~len:3
    ; firm = Sbe.get_string b ~pos:(base + 71) ~len:5
    }
  ;;
end

module Negotiation_response = struct
  type t =
    { uuid : int
    ; request_timestamp : int
    ; secret_key_secure_id_expiration : int option
    ; previous_seq_no : int
    ; previous_uuid : int
    }
  [@@deriving sexp, compare, equal]

  let template_id = 501
  let block_length = 32

  let write b ~base t =
    Sbe.set_u64 b ~pos:base t.uuid;
    Sbe.set_u64 b ~pos:(base + 8) t.request_timestamp;
    Sbe.set_u16_opt b ~pos:(base + 16) t.secret_key_secure_id_expiration;
    set_null_u8 b ~pos:(base + 18);
    set_null_u8 b ~pos:(base + 19);
    Sbe.set_u32 b ~pos:(base + 20) t.previous_seq_no;
    Sbe.set_u64 b ~pos:(base + 24) t.previous_uuid
  ;;

  let read b ~base =
    { uuid = Sbe.get_u64 b ~pos:base
    ; request_timestamp = Sbe.get_u64 b ~pos:(base + 8)
    ; secret_key_secure_id_expiration = Sbe.get_u16_opt b ~pos:(base + 16)
    ; previous_seq_no = Sbe.get_u32 b ~pos:(base + 20)
    ; previous_uuid = Sbe.get_u64 b ~pos:(base + 24)
    }
  ;;
end

module Negotiation_reject = struct
  type t =
    { reason : string
    ; uuid : int
    ; request_timestamp : int
    ; error_codes : int
    }
  [@@deriving sexp, compare, equal]

  let template_id = 502
  let block_length = 68

  let write b ~base t =
    Sbe.set_string b ~pos:base ~len:48 t.reason;
    Sbe.set_u64 b ~pos:(base + 48) t.uuid;
    Sbe.set_u64 b ~pos:(base + 56) t.request_timestamp;
    Sbe.set_u16 b ~pos:(base + 64) t.error_codes;
    set_null_u8 b ~pos:(base + 66);
    set_null_u8 b ~pos:(base + 67)
  ;;

  let read b ~base =
    { reason = Sbe.get_string b ~pos:base ~len:48
    ; uuid = Sbe.get_u64 b ~pos:(base + 48)
    ; request_timestamp = Sbe.get_u64 b ~pos:(base + 56)
    ; error_codes = Sbe.get_u16 b ~pos:(base + 64)
    }
  ;;
end

module Establish = struct
  type t =
    { hmac_signature : string
    ; access_key_id : string
    ; trading_system_name : string
    ; trading_system_version : string
    ; trading_system_vendor : string
    ; uuid : int
    ; request_timestamp : int
    ; next_seq_no : int
    ; session : string
    ; firm : string
    ; keep_alive_interval : int
    }
  [@@deriving sexp, compare, equal]

  let template_id = 503
  let block_length = 132

  let write b ~base t =
    Sbe.set_string b ~pos:base ~len:32 t.hmac_signature;
    Sbe.set_string b ~pos:(base + 32) ~len:20 t.access_key_id;
    Sbe.set_string b ~pos:(base + 52) ~len:30 t.trading_system_name;
    Sbe.set_string b ~pos:(base + 82) ~len:10 t.trading_system_version;
    Sbe.set_string b ~pos:(base + 92) ~len:10 t.trading_system_vendor;
    Sbe.set_u64 b ~pos:(base + 102) t.uuid;
    Sbe.set_u64 b ~pos:(base + 110) t.request_timestamp;
    Sbe.set_u32 b ~pos:(base + 118) t.next_seq_no;
    Sbe.set_string b ~pos:(base + 122) ~len:3 t.session;
    Sbe.set_string b ~pos:(base + 125) ~len:5 t.firm;
    Sbe.set_u16 b ~pos:(base + 130) t.keep_alive_interval
  ;;

  let read b ~base =
    { hmac_signature = Sbe.get_string b ~pos:base ~len:32
    ; access_key_id = Sbe.get_string b ~pos:(base + 32) ~len:20
    ; trading_system_name = Sbe.get_string b ~pos:(base + 52) ~len:30
    ; trading_system_version = Sbe.get_string b ~pos:(base + 82) ~len:10
    ; trading_system_vendor = Sbe.get_string b ~pos:(base + 92) ~len:10
    ; uuid = Sbe.get_u64 b ~pos:(base + 102)
    ; request_timestamp = Sbe.get_u64 b ~pos:(base + 110)
    ; next_seq_no = Sbe.get_u32 b ~pos:(base + 118)
    ; session = Sbe.get_string b ~pos:(base + 122) ~len:3
    ; firm = Sbe.get_string b ~pos:(base + 125) ~len:5
    ; keep_alive_interval = Sbe.get_u16 b ~pos:(base + 130)
    }
  ;;
end

module Establishment_ack = struct
  type t =
    { uuid : int
    ; request_timestamp : int
    ; next_seq_no : int
    ; previous_seq_no : int
    ; previous_uuid : int
    ; keep_alive_interval : int
    ; secret_key_secure_id_expiration : int option
    }
  [@@deriving sexp, compare, equal]

  let template_id = 504
  let block_length = 38

  let write b ~base t =
    Sbe.set_u64 b ~pos:base t.uuid;
    Sbe.set_u64 b ~pos:(base + 8) t.request_timestamp;
    Sbe.set_u32 b ~pos:(base + 16) t.next_seq_no;
    Sbe.set_u32 b ~pos:(base + 20) t.previous_seq_no;
    Sbe.set_u64 b ~pos:(base + 24) t.previous_uuid;
    Sbe.set_u16 b ~pos:(base + 32) t.keep_alive_interval;
    Sbe.set_u16_opt b ~pos:(base + 34) t.secret_key_secure_id_expiration;
    set_null_u8 b ~pos:(base + 36);
    set_null_u8 b ~pos:(base + 37)
  ;;

  let read b ~base =
    { uuid = Sbe.get_u64 b ~pos:base
    ; request_timestamp = Sbe.get_u64 b ~pos:(base + 8)
    ; next_seq_no = Sbe.get_u32 b ~pos:(base + 16)
    ; previous_seq_no = Sbe.get_u32 b ~pos:(base + 20)
    ; previous_uuid = Sbe.get_u64 b ~pos:(base + 24)
    ; keep_alive_interval = Sbe.get_u16 b ~pos:(base + 32)
    ; secret_key_secure_id_expiration = Sbe.get_u16_opt b ~pos:(base + 34)
    }
  ;;
end

module Establishment_reject = struct
  type t =
    { reason : string
    ; uuid : int
    ; request_timestamp : int
    ; next_seq_no : int
    ; error_codes : int
    }
  [@@deriving sexp, compare, equal]

  let template_id = 505
  let block_length = 72

  let write b ~base t =
    Sbe.set_string b ~pos:base ~len:48 t.reason;
    Sbe.set_u64 b ~pos:(base + 48) t.uuid;
    Sbe.set_u64 b ~pos:(base + 56) t.request_timestamp;
    Sbe.set_u32 b ~pos:(base + 64) t.next_seq_no;
    Sbe.set_u16 b ~pos:(base + 68) t.error_codes;
    set_null_u8 b ~pos:(base + 70);
    set_null_u8 b ~pos:(base + 71)
  ;;

  let read b ~base =
    { reason = Sbe.get_string b ~pos:base ~len:48
    ; uuid = Sbe.get_u64 b ~pos:(base + 48)
    ; request_timestamp = Sbe.get_u64 b ~pos:(base + 56)
    ; next_seq_no = Sbe.get_u32 b ~pos:(base + 64)
    ; error_codes = Sbe.get_u16 b ~pos:(base + 68)
    }
  ;;
end

module Sequence = struct
  type t =
    { uuid : int
    ; next_seq_no : int
    ; keep_alive_interval_lapsed : bool
    }
  [@@deriving sexp, compare, equal]

  let template_id = 506
  let block_length = 14

  let write b ~base t =
    Sbe.set_u64 b ~pos:base t.uuid;
    Sbe.set_u32 b ~pos:(base + 8) t.next_seq_no;
    set_null_u8 b ~pos:(base + 12);
    Sbe.set_bool b ~pos:(base + 13) t.keep_alive_interval_lapsed
  ;;

  let read b ~base =
    { uuid = Sbe.get_u64 b ~pos:base
    ; next_seq_no = Sbe.get_u32 b ~pos:(base + 8)
    ; keep_alive_interval_lapsed = Sbe.get_bool b ~pos:(base + 13)
    }
  ;;
end

module Terminate = struct
  type t =
    { reason : string
    ; uuid : int
    ; request_timestamp : int
    ; error_codes : int
    }
  [@@deriving sexp, compare, equal]

  let template_id = 507
  let block_length = 67

  let write b ~base t =
    Sbe.set_string b ~pos:base ~len:48 t.reason;
    Sbe.set_u64 b ~pos:(base + 48) t.uuid;
    Sbe.set_u64 b ~pos:(base + 56) t.request_timestamp;
    Sbe.set_u16 b ~pos:(base + 64) t.error_codes;
    set_null_u8 b ~pos:(base + 66)
  ;;

  let read b ~base =
    { reason = Sbe.get_string b ~pos:base ~len:48
    ; uuid = Sbe.get_u64 b ~pos:(base + 48)
    ; request_timestamp = Sbe.get_u64 b ~pos:(base + 56)
    ; error_codes = Sbe.get_u16 b ~pos:(base + 64)
    }
  ;;
end

module Retransmit_request = struct
  type t =
    { uuid : int
    ; last_uuid : int option
    ; request_timestamp : int
    ; from_seq_no : int
    ; msg_count : int
    }
  [@@deriving sexp, compare, equal]

  let template_id = 508
  let block_length = 30

  let write b ~base t =
    Sbe.set_u64 b ~pos:base t.uuid;
    Sbe.set_u64_opt b ~pos:(base + 8) t.last_uuid;
    Sbe.set_u64 b ~pos:(base + 16) t.request_timestamp;
    Sbe.set_u32 b ~pos:(base + 24) t.from_seq_no;
    Sbe.set_u16 b ~pos:(base + 28) t.msg_count
  ;;

  let read b ~base =
    { uuid = Sbe.get_u64 b ~pos:base
    ; last_uuid = Sbe.get_u64_opt b ~pos:(base + 8)
    ; request_timestamp = Sbe.get_u64 b ~pos:(base + 16)
    ; from_seq_no = Sbe.get_u32 b ~pos:(base + 24)
    ; msg_count = Sbe.get_u16 b ~pos:(base + 28)
    }
  ;;
end

module Retransmission = struct
  type t = Retransmit_request.t =
    { uuid : int
    ; last_uuid : int option
    ; request_timestamp : int
    ; from_seq_no : int
    ; msg_count : int
    }
  [@@deriving sexp, compare, equal]

  let template_id = 509
  let block_length = 31

  let write b ~base t =
    Retransmit_request.write b ~base t;
    set_null_u8 b ~pos:(base + 30)
  ;;

  let read = Retransmit_request.read
end

module Retransmit_reject = struct
  type t =
    { reason : string
    ; uuid : int
    ; last_uuid : int option
    ; request_timestamp : int
    ; error_codes : int
    }
  [@@deriving sexp, compare, equal]

  let template_id = 510
  let block_length = 75

  let write b ~base t =
    Sbe.set_string b ~pos:base ~len:48 t.reason;
    Sbe.set_u64 b ~pos:(base + 48) t.uuid;
    Sbe.set_u64_opt b ~pos:(base + 56) t.last_uuid;
    Sbe.set_u64 b ~pos:(base + 64) t.request_timestamp;
    Sbe.set_u16 b ~pos:(base + 72) t.error_codes;
    set_null_u8 b ~pos:(base + 74)
  ;;

  let read b ~base =
    { reason = Sbe.get_string b ~pos:base ~len:48
    ; uuid = Sbe.get_u64 b ~pos:(base + 48)
    ; last_uuid = Sbe.get_u64_opt b ~pos:(base + 56)
    ; request_timestamp = Sbe.get_u64 b ~pos:(base + 64)
    ; error_codes = Sbe.get_u16 b ~pos:(base + 72)
    }
  ;;
end

module Not_applied = struct
  type t =
    { uuid : int
    ; from_seq_no : int
    ; msg_count : int
    }
  [@@deriving sexp, compare, equal]

  let template_id = 513
  let block_length = 17

  let write b ~base t =
    Sbe.set_u64 b ~pos:base t.uuid;
    Sbe.set_u32 b ~pos:(base + 8) t.from_seq_no;
    Sbe.set_u32 b ~pos:(base + 12) t.msg_count;
    set_null_u8 b ~pos:(base + 16)
  ;;

  let read b ~base =
    { uuid = Sbe.get_u64 b ~pos:base
    ; from_seq_no = Sbe.get_u32 b ~pos:(base + 8)
    ; msg_count = Sbe.get_u32 b ~pos:(base + 12)
    }
  ;;
end

module New_order_single = struct
  type t =
    { price : Price.t option
    ; order_qty : int
    ; security_id : int
    ; side : Side.t
    ; seq_num : int
    ; sender_id : string
    ; cl_ord_id : string
    ; party_details_list_req_id : int
    ; order_request_id : int
    ; sending_time : int
    ; stop_px : Price.t option
    ; location : string
    ; min_qty : int option
    ; display_qty : int option
    ; ord_type : Ord_type.t
    ; time_in_force : Time_in_force.t
    ; manual : bool
    }
  [@@deriving sexp, compare, equal]

  let template_id = 514
  let block_length = 116

  let write b ~base t =
    Sbe.set_price_opt b ~pos:base t.price;
    Sbe.set_u32 b ~pos:(base + 8) t.order_qty;
    Sbe.set_i32 b ~pos:(base + 12) t.security_id;
    Sbe.set_u8 b ~pos:(base + 16) (Side.to_int t.side);
    Sbe.set_u32 b ~pos:(base + 17) t.seq_num;
    Sbe.set_string b ~pos:(base + 21) ~len:20 t.sender_id;
    Sbe.set_string b ~pos:(base + 41) ~len:20 t.cl_ord_id;
    Sbe.set_u64 b ~pos:(base + 61) t.party_details_list_req_id;
    Sbe.set_u64 b ~pos:(base + 69) t.order_request_id;
    Sbe.set_u64 b ~pos:(base + 77) t.sending_time;
    Sbe.set_price_opt b ~pos:(base + 85) t.stop_px;
    Sbe.set_string b ~pos:(base + 93) ~len:5 t.location;
    Sbe.set_u32_opt b ~pos:(base + 98) t.min_qty;
    Sbe.set_u32_opt b ~pos:(base + 102) t.display_qty;
    Sbe.set_u16_opt b ~pos:(base + 106) None;
    Sbe.set_char b ~pos:(base + 108) (Ord_type.to_char t.ord_type);
    Sbe.set_u8 b ~pos:(base + 109) (Time_in_force.to_int t.time_in_force);
    Sbe.set_bool b ~pos:(base + 110) t.manual;
    (* ExecInst @111 empty set, ExecutionMode @112 charNULL *)
    set_null_u8 b ~pos:(base + 113);
    set_null_u8 b ~pos:(base + 114);
    set_null_u8 b ~pos:(base + 115)
  ;;

  let read b ~base =
    { price = Sbe.get_price_opt b ~pos:base
    ; order_qty = Sbe.get_u32 b ~pos:(base + 8)
    ; security_id = Sbe.get_i32 b ~pos:(base + 12)
    ; side = decode_side b ~pos:(base + 16)
    ; seq_num = Sbe.get_u32 b ~pos:(base + 17)
    ; sender_id = Sbe.get_string b ~pos:(base + 21) ~len:20
    ; cl_ord_id = Sbe.get_string b ~pos:(base + 41) ~len:20
    ; party_details_list_req_id = Sbe.get_u64 b ~pos:(base + 61)
    ; order_request_id = Sbe.get_u64 b ~pos:(base + 69)
    ; sending_time = Sbe.get_u64 b ~pos:(base + 77)
    ; stop_px = Sbe.get_price_opt b ~pos:(base + 85)
    ; location = Sbe.get_string b ~pos:(base + 93) ~len:5
    ; min_qty = Sbe.get_u32_opt b ~pos:(base + 98)
    ; display_qty = Sbe.get_u32_opt b ~pos:(base + 102)
    ; ord_type = decode_ord_type b ~pos:(base + 108)
    ; time_in_force = decode_tif b ~pos:(base + 109)
    ; manual = Sbe.get_bool b ~pos:(base + 110)
    }
  ;;
end

module Order_cancel_replace_request = struct
  type t =
    { price : Price.t option
    ; order_qty : int
    ; security_id : int
    ; side : Side.t
    ; seq_num : int
    ; sender_id : string
    ; cl_ord_id : string
    ; party_details_list_req_id : int
    ; order_id : int
    ; stop_px : Price.t option
    ; order_request_id : int
    ; sending_time : int
    ; location : string
    ; min_qty : int option
    ; display_qty : int option
    ; ord_type : Ord_type.t
    ; time_in_force : Time_in_force.t
    ; manual : bool
    ; ofm_override : bool
    }
  [@@deriving sexp, compare, equal]

  let template_id = 515
  let block_length = 125

  let write b ~base t =
    Sbe.set_price_opt b ~pos:base t.price;
    Sbe.set_u32 b ~pos:(base + 8) t.order_qty;
    Sbe.set_i32 b ~pos:(base + 12) t.security_id;
    Sbe.set_u8 b ~pos:(base + 16) (Side.to_int t.side);
    Sbe.set_u32 b ~pos:(base + 17) t.seq_num;
    Sbe.set_string b ~pos:(base + 21) ~len:20 t.sender_id;
    Sbe.set_string b ~pos:(base + 41) ~len:20 t.cl_ord_id;
    Sbe.set_u64 b ~pos:(base + 61) t.party_details_list_req_id;
    Sbe.set_u64 b ~pos:(base + 69) t.order_id;
    Sbe.set_price_opt b ~pos:(base + 77) t.stop_px;
    Sbe.set_u64 b ~pos:(base + 85) t.order_request_id;
    Sbe.set_u64 b ~pos:(base + 93) t.sending_time;
    Sbe.set_string b ~pos:(base + 101) ~len:5 t.location;
    Sbe.set_u32_opt b ~pos:(base + 106) t.min_qty;
    Sbe.set_u32_opt b ~pos:(base + 110) t.display_qty;
    Sbe.set_u16_opt b ~pos:(base + 114) None;
    Sbe.set_char b ~pos:(base + 116) (Ord_type.to_char t.ord_type);
    Sbe.set_u8 b ~pos:(base + 117) (Time_in_force.to_int t.time_in_force);
    Sbe.set_bool b ~pos:(base + 118) t.manual;
    Sbe.set_bool b ~pos:(base + 119) t.ofm_override;
    set_null_u8 b ~pos:(base + 122);
    set_null_u8 b ~pos:(base + 123);
    set_null_u8 b ~pos:(base + 124)
  ;;

  let read b ~base =
    { price = Sbe.get_price_opt b ~pos:base
    ; order_qty = Sbe.get_u32 b ~pos:(base + 8)
    ; security_id = Sbe.get_i32 b ~pos:(base + 12)
    ; side = decode_side b ~pos:(base + 16)
    ; seq_num = Sbe.get_u32 b ~pos:(base + 17)
    ; sender_id = Sbe.get_string b ~pos:(base + 21) ~len:20
    ; cl_ord_id = Sbe.get_string b ~pos:(base + 41) ~len:20
    ; party_details_list_req_id = Sbe.get_u64 b ~pos:(base + 61)
    ; order_id = Sbe.get_u64 b ~pos:(base + 69)
    ; stop_px = Sbe.get_price_opt b ~pos:(base + 77)
    ; order_request_id = Sbe.get_u64 b ~pos:(base + 85)
    ; sending_time = Sbe.get_u64 b ~pos:(base + 93)
    ; location = Sbe.get_string b ~pos:(base + 101) ~len:5
    ; min_qty = Sbe.get_u32_opt b ~pos:(base + 106)
    ; display_qty = Sbe.get_u32_opt b ~pos:(base + 110)
    ; ord_type = decode_ord_type b ~pos:(base + 116)
    ; time_in_force = decode_tif b ~pos:(base + 117)
    ; manual = Sbe.get_bool b ~pos:(base + 118)
    ; ofm_override = Sbe.get_bool b ~pos:(base + 119)
    }
  ;;
end

module Order_cancel_request = struct
  type t =
    { order_id : int
    ; party_details_list_req_id : int
    ; manual : bool
    ; seq_num : int
    ; sender_id : string
    ; cl_ord_id : string
    ; order_request_id : int
    ; sending_time : int
    ; location : string
    ; security_id : int
    ; side : Side.t
    }
  [@@deriving sexp, compare, equal]

  let template_id = 516
  let block_length = 88

  let write b ~base t =
    Sbe.set_u64 b ~pos:base t.order_id;
    Sbe.set_u64 b ~pos:(base + 8) t.party_details_list_req_id;
    Sbe.set_bool b ~pos:(base + 16) t.manual;
    Sbe.set_u32 b ~pos:(base + 17) t.seq_num;
    Sbe.set_string b ~pos:(base + 21) ~len:20 t.sender_id;
    Sbe.set_string b ~pos:(base + 41) ~len:20 t.cl_ord_id;
    Sbe.set_u64 b ~pos:(base + 61) t.order_request_id;
    Sbe.set_u64 b ~pos:(base + 69) t.sending_time;
    Sbe.set_string b ~pos:(base + 77) ~len:5 t.location;
    Sbe.set_i32 b ~pos:(base + 82) t.security_id;
    Sbe.set_u8 b ~pos:(base + 86) (Side.to_int t.side);
    set_null_u8 b ~pos:(base + 87)
  ;;

  let read b ~base =
    { order_id = Sbe.get_u64 b ~pos:base
    ; party_details_list_req_id = Sbe.get_u64 b ~pos:(base + 8)
    ; manual = Sbe.get_bool b ~pos:(base + 16)
    ; seq_num = Sbe.get_u32 b ~pos:(base + 17)
    ; sender_id = Sbe.get_string b ~pos:(base + 21) ~len:20
    ; cl_ord_id = Sbe.get_string b ~pos:(base + 41) ~len:20
    ; order_request_id = Sbe.get_u64 b ~pos:(base + 61)
    ; sending_time = Sbe.get_u64 b ~pos:(base + 69)
    ; location = Sbe.get_string b ~pos:(base + 77) ~len:5
    ; security_id = Sbe.get_i32 b ~pos:(base + 82)
    ; side = decode_side b ~pos:(base + 86)
    }
  ;;
end

module Exec_common = struct
  type t =
    { seq_num : int
    ; uuid : int
    ; exec_id : string
    ; sender_id : string
    ; cl_ord_id : string
    ; party_details_list_req_id : int
    ; order_id : int
    ; transact_time : int
    ; sending_time : int
    ; order_request_id : int
    ; location : string
    ; security_id : int
    ; side : Side.t
    ; manual : bool
    ; poss_retrans : bool
    }
  [@@deriving sexp, compare, equal]

  (* Offsets of the shared fields in one template. SenderID, ClOrdID and
     PartyDetailsListReqID follow ExecID at +40, +60 and +80; SendingTimeEpoch and
     OrderRequestID follow TransactTime at +8 and +16. SeqNum and UUID are always at 0 and

     4. *)
  type layout =
    { exec_id_at : int
    ; order_id_at : int
    ; transact_time_at : int
    ; location_at : int
    ; security_id_at : int
    ; side_at : int
    ; manual_at : int
    ; poss_retrans_at : int
    }

  let write l b ~base t =
    Sbe.set_u32 b ~pos:base t.seq_num;
    Sbe.set_u64 b ~pos:(base + 4) t.uuid;
    Sbe.set_string b ~pos:(base + l.exec_id_at) ~len:40 t.exec_id;
    Sbe.set_string b ~pos:(base + l.exec_id_at + 40) ~len:20 t.sender_id;
    Sbe.set_string b ~pos:(base + l.exec_id_at + 60) ~len:20 t.cl_ord_id;
    Sbe.set_u64 b ~pos:(base + l.exec_id_at + 80) t.party_details_list_req_id;
    Sbe.set_u64 b ~pos:(base + l.order_id_at) t.order_id;
    Sbe.set_u64 b ~pos:(base + l.transact_time_at) t.transact_time;
    Sbe.set_u64 b ~pos:(base + l.transact_time_at + 8) t.sending_time;
    Sbe.set_u64 b ~pos:(base + l.transact_time_at + 16) t.order_request_id;
    Sbe.set_string b ~pos:(base + l.location_at) ~len:5 t.location;
    Sbe.set_i32 b ~pos:(base + l.security_id_at) t.security_id;
    Sbe.set_u8 b ~pos:(base + l.side_at) (Side.to_int t.side);
    Sbe.set_bool b ~pos:(base + l.manual_at) t.manual;
    Sbe.set_bool b ~pos:(base + l.poss_retrans_at) t.poss_retrans
  ;;

  let read l b ~base =
    { seq_num = Sbe.get_u32 b ~pos:base
    ; uuid = Sbe.get_u64 b ~pos:(base + 4)
    ; exec_id = Sbe.get_string b ~pos:(base + l.exec_id_at) ~len:40
    ; sender_id = Sbe.get_string b ~pos:(base + l.exec_id_at + 40) ~len:20
    ; cl_ord_id = Sbe.get_string b ~pos:(base + l.exec_id_at + 60) ~len:20
    ; party_details_list_req_id = Sbe.get_u64 b ~pos:(base + l.exec_id_at + 80)
    ; order_id = Sbe.get_u64 b ~pos:(base + l.order_id_at)
    ; transact_time = Sbe.get_u64 b ~pos:(base + l.transact_time_at)
    ; sending_time = Sbe.get_u64 b ~pos:(base + l.transact_time_at + 8)
    ; order_request_id = Sbe.get_u64 b ~pos:(base + l.transact_time_at + 16)
    ; location = Sbe.get_string b ~pos:(base + l.location_at) ~len:5
    ; security_id = Sbe.get_i32 b ~pos:(base + l.security_id_at)
    ; side = decode_side b ~pos:(base + l.side_at)
    ; manual = Sbe.get_bool b ~pos:(base + l.manual_at)
    ; poss_retrans = Sbe.get_bool b ~pos:(base + l.poss_retrans_at)
    }
  ;;
end

(* Writes the null value of several 1-byte optional fields. *)
let nulls_u8 b ~base offsets =
  List.iter offsets ~f:(fun o -> set_null_u8 b ~pos:(base + o))
;;

let null_u64 b ~pos = Sbe.set_u64_opt b ~pos None

module Business_reject = struct
  type t =
    { seq_num : int
    ; uuid : int
    ; text : string
    ; sender_id : string
    ; party_details_list_req_id : int option
    ; sending_time : int
    ; business_reject_ref_id : int option
    ; location : string
    ; ref_seq_num : int option
    ; ref_tag_id : int option
    ; business_reject_reason : int
    ; ref_msg_type : string
    ; poss_retrans : bool
    }
  [@@deriving sexp, compare, equal]

  let template_id = 521
  let block_length = 330

  let write b ~base t =
    Sbe.set_u32 b ~pos:base t.seq_num;
    Sbe.set_u64 b ~pos:(base + 4) t.uuid;
    Sbe.set_string b ~pos:(base + 12) ~len:256 t.text;
    Sbe.set_string b ~pos:(base + 268) ~len:20 t.sender_id;
    Sbe.set_u64_opt b ~pos:(base + 288) t.party_details_list_req_id;
    Sbe.set_u64 b ~pos:(base + 296) t.sending_time;
    Sbe.set_u64_opt b ~pos:(base + 304) t.business_reject_ref_id;
    Sbe.set_string b ~pos:(base + 312) ~len:5 t.location;
    Sbe.set_u32_opt b ~pos:(base + 317) t.ref_seq_num;
    Sbe.set_u16_opt b ~pos:(base + 321) t.ref_tag_id;
    Sbe.set_u16 b ~pos:(base + 323) t.business_reject_reason;
    Sbe.set_string b ~pos:(base + 325) ~len:2 t.ref_msg_type;
    Sbe.set_bool b ~pos:(base + 327) t.poss_retrans;
    nulls_u8 b ~base [ 328; 329 ]
  ;;

  let read b ~base =
    { seq_num = Sbe.get_u32 b ~pos:base
    ; uuid = Sbe.get_u64 b ~pos:(base + 4)
    ; text = Sbe.get_string b ~pos:(base + 12) ~len:256
    ; sender_id = Sbe.get_string b ~pos:(base + 268) ~len:20
    ; party_details_list_req_id = Sbe.get_u64_opt b ~pos:(base + 288)
    ; sending_time = Sbe.get_u64 b ~pos:(base + 296)
    ; business_reject_ref_id = Sbe.get_u64_opt b ~pos:(base + 304)
    ; location = Sbe.get_string b ~pos:(base + 312) ~len:5
    ; ref_seq_num = Sbe.get_u32_opt b ~pos:(base + 317)
    ; ref_tag_id = Sbe.get_u16_opt b ~pos:(base + 321)
    ; business_reject_reason = Sbe.get_u16 b ~pos:(base + 323)
    ; ref_msg_type = Sbe.get_string b ~pos:(base + 325) ~len:2
    ; poss_retrans = Sbe.get_bool b ~pos:(base + 327)
    }
  ;;
end

module Execution_report_new = struct
  type t =
    { common : Exec_common.t
    ; price : Price.t
    ; stop_px : Price.t option
    ; order_qty : int
    ; min_qty : int option
    ; display_qty : int option
    ; ord_type : Ord_type.t
    ; time_in_force : Time_in_force.t
    }
  [@@deriving sexp, compare, equal]

  let template_id = 522
  let block_length = 209

  let layout : Exec_common.layout =
    { exec_id_at = 12
    ; order_id_at = 100
    ; transact_time_at = 124
    ; location_at = 164
    ; security_id_at = 169
    ; side_at = 190
    ; manual_at = 192
    ; poss_retrans_at = 193
    }
  ;;

  let write b ~base t =
    Exec_common.write layout b ~base t.common;
    Sbe.set_price b ~pos:(base + 108) t.price;
    Sbe.set_price_opt b ~pos:(base + 116) t.stop_px;
    null_u64 b ~pos:(base + 148);
    null_u64 b ~pos:(base + 156);
    Sbe.set_u32 b ~pos:(base + 173) t.order_qty;
    Sbe.set_u32_opt b ~pos:(base + 177) t.min_qty;
    Sbe.set_u32_opt b ~pos:(base + 181) t.display_qty;
    Sbe.set_u16_opt b ~pos:(base + 185) None;
    Sbe.set_u16_opt b ~pos:(base + 187) None;
    Sbe.set_char b ~pos:(base + 189) (Ord_type.to_char t.ord_type);
    Sbe.set_u8 b ~pos:(base + 191) (Time_in_force.to_int t.time_in_force);
    nulls_u8 b ~base [ 194; 195; 198; 199; 200 ];
    null_u64 b ~pos:(base + 201)
  ;;

  let read b ~base =
    { common = Exec_common.read layout b ~base
    ; price = Sbe.get_price b ~pos:(base + 108)
    ; stop_px = Sbe.get_price_opt b ~pos:(base + 116)
    ; order_qty = Sbe.get_u32 b ~pos:(base + 173)
    ; min_qty = Sbe.get_u32_opt b ~pos:(base + 177)
    ; display_qty = Sbe.get_u32_opt b ~pos:(base + 181)
    ; ord_type = decode_ord_type b ~pos:(base + 189)
    ; time_in_force = decode_tif b ~pos:(base + 191)
    }
  ;;
end

module Execution_report_reject = struct
  type t =
    { common : Exec_common.t
    ; text : string
    ; price : Price.t option
    ; stop_px : Price.t option
    ; order_qty : int
    ; min_qty : int option
    ; display_qty : int option
    ; ord_rej_reason : int
    ; ord_type : Ord_type.t
    ; time_in_force : Time_in_force.t
    }
  [@@deriving sexp, compare, equal]

  let template_id = 523
  let block_length = 467

  let layout : Exec_common.layout =
    { exec_id_at = 268
    ; order_id_at = 356
    ; transact_time_at = 380
    ; location_at = 420
    ; security_id_at = 425
    ; side_at = 448
    ; manual_at = 450
    ; poss_retrans_at = 451
    }
  ;;

  let write b ~base t =
    Exec_common.write layout b ~base t.common;
    Sbe.set_string b ~pos:(base + 12) ~len:256 t.text;
    Sbe.set_price_opt b ~pos:(base + 364) t.price;
    Sbe.set_price_opt b ~pos:(base + 372) t.stop_px;
    null_u64 b ~pos:(base + 404);
    null_u64 b ~pos:(base + 412);
    Sbe.set_u32 b ~pos:(base + 429) t.order_qty;
    Sbe.set_u32_opt b ~pos:(base + 433) t.min_qty;
    Sbe.set_u32_opt b ~pos:(base + 437) t.display_qty;
    Sbe.set_u16 b ~pos:(base + 441) t.ord_rej_reason;
    Sbe.set_u16_opt b ~pos:(base + 443) None;
    Sbe.set_u16_opt b ~pos:(base + 445) None;
    Sbe.set_char b ~pos:(base + 447) (Ord_type.to_char t.ord_type);
    Sbe.set_u8 b ~pos:(base + 449) (Time_in_force.to_int t.time_in_force);
    nulls_u8 b ~base [ 452; 453; 456; 457; 458 ];
    null_u64 b ~pos:(base + 459)
  ;;

  let read b ~base =
    { common = Exec_common.read layout b ~base
    ; text = Sbe.get_string b ~pos:(base + 12) ~len:256
    ; price = Sbe.get_price_opt b ~pos:(base + 364)
    ; stop_px = Sbe.get_price_opt b ~pos:(base + 372)
    ; order_qty = Sbe.get_u32 b ~pos:(base + 429)
    ; min_qty = Sbe.get_u32_opt b ~pos:(base + 433)
    ; display_qty = Sbe.get_u32_opt b ~pos:(base + 437)
    ; ord_rej_reason = Sbe.get_u16 b ~pos:(base + 441)
    ; ord_type = decode_ord_type b ~pos:(base + 447)
    ; time_in_force = decode_tif b ~pos:(base + 449)
    }
  ;;
end

module Execution_report_elimination = struct
  type t =
    { common : Exec_common.t
    ; price : Price.t
    ; cum_qty : int
    ; order_qty : int
    ; min_qty : int option
    ; display_qty : int option
    ; ord_type : Ord_type.t
    ; time_in_force : Time_in_force.t
    }
  [@@deriving sexp, compare, equal]

  let template_id = 524
  let block_length = 202

  let layout : Exec_common.layout =
    { exec_id_at = 12
    ; order_id_at = 100
    ; transact_time_at = 124
    ; location_at = 164
    ; security_id_at = 169
    ; side_at = 192
    ; manual_at = 194
    ; poss_retrans_at = 195
    }
  ;;

  let write b ~base t =
    Exec_common.write layout b ~base t.common;
    Sbe.set_price b ~pos:(base + 108) t.price;
    Sbe.set_price_opt b ~pos:(base + 116) None;
    null_u64 b ~pos:(base + 148);
    null_u64 b ~pos:(base + 156);
    Sbe.set_u32 b ~pos:(base + 173) t.cum_qty;
    Sbe.set_u32 b ~pos:(base + 177) t.order_qty;
    Sbe.set_u32_opt b ~pos:(base + 181) t.min_qty;
    Sbe.set_u32_opt b ~pos:(base + 185) t.display_qty;
    Sbe.set_u16_opt b ~pos:(base + 189) None;
    Sbe.set_char b ~pos:(base + 191) (Ord_type.to_char t.ord_type);
    Sbe.set_u8 b ~pos:(base + 193) (Time_in_force.to_int t.time_in_force);
    nulls_u8 b ~base [ 196; 199; 200; 201 ]
  ;;

  let read b ~base =
    { common = Exec_common.read layout b ~base
    ; price = Sbe.get_price b ~pos:(base + 108)
    ; cum_qty = Sbe.get_u32 b ~pos:(base + 173)
    ; order_qty = Sbe.get_u32 b ~pos:(base + 177)
    ; min_qty = Sbe.get_u32_opt b ~pos:(base + 181)
    ; display_qty = Sbe.get_u32_opt b ~pos:(base + 185)
    ; ord_type = decode_ord_type b ~pos:(base + 191)
    ; time_in_force = decode_tif b ~pos:(base + 193)
    }
  ;;
end

module Fill = struct
  type t =
    { fill_px : Price.t
    ; fill_qty : int
    ; fill_exec_id : string
    ; fill_yield_type : int
    }
  [@@deriving sexp, compare, equal]

  let block_length = 15

  let write b ~pos t =
    Sbe.set_price b ~pos t.fill_px;
    Sbe.set_u32 b ~pos:(pos + 8) t.fill_qty;
    Sbe.set_string b ~pos:(pos + 12) ~len:2 t.fill_exec_id;
    Sbe.set_u8 b ~pos:(pos + 14) t.fill_yield_type
  ;;

  let read b ~pos =
    { fill_px = Sbe.get_price b ~pos
    ; fill_qty = Sbe.get_u32 b ~pos:(pos + 8)
    ; fill_exec_id = Sbe.get_string b ~pos:(pos + 12) ~len:2
    ; fill_yield_type = Sbe.get_u8 b ~pos:(pos + 14)
    }
  ;;
end

module Execution_report_trade_outright = struct
  module Ord_status = struct
    type t =
      | Partially_filled
      | Filled
    [@@deriving sexp, compare, equal]
  end

  type t =
    { common : Exec_common.t
    ; last_px : Price.t
    ; price : Price.t
    ; sec_exec_id : int
    ; order_qty : int
    ; last_qty : int
    ; cum_qty : int
    ; md_trade_entry_id : int
    ; side_trade_id : int
    ; leaves_qty : int
    ; trade_date : int option
    ; ord_status : Ord_status.t
    ; ord_type : Ord_type.t
    ; time_in_force : Time_in_force.t
    ; aggressor : bool
    ; fills : Fill.t list
    }
  [@@deriving sexp, compare, equal]

  let template_id = 525
  let block_length = 235
  let order_event_block_length = 23

  (* NoFills and NoOrderEvents group headers; no order events are sent. *)
  let extra t = (2 * Sbe.Group.size) + (Fill.block_length * List.length t.fills)

  let layout : Exec_common.layout =
    { exec_id_at = 12
    ; order_id_at = 108
    ; transact_time_at = 132
    ; location_at = 180
    ; security_id_at = 185
    ; side_at = 223
    ; manual_at = 225
    ; poss_retrans_at = 226
    }
  ;;

  let write b ~base t =
    Exec_common.write layout b ~base t.common;
    Sbe.set_price b ~pos:(base + 100) t.last_px;
    Sbe.set_price b ~pos:(base + 116) t.price;
    Sbe.set_price_opt b ~pos:(base + 124) None;
    Sbe.set_u64 b ~pos:(base + 156) t.sec_exec_id;
    null_u64 b ~pos:(base + 164);
    null_u64 b ~pos:(base + 172);
    Sbe.set_u32 b ~pos:(base + 189) t.order_qty;
    Sbe.set_u32 b ~pos:(base + 193) t.last_qty;
    Sbe.set_u32 b ~pos:(base + 197) t.cum_qty;
    Sbe.set_u32 b ~pos:(base + 201) t.md_trade_entry_id;
    Sbe.set_u32 b ~pos:(base + 205) t.side_trade_id;
    Sbe.set_u32_opt b ~pos:(base + 209) None;
    Sbe.set_u32 b ~pos:(base + 213) t.leaves_qty;
    Sbe.set_u16_opt b ~pos:(base + 217) t.trade_date;
    Sbe.set_u16_opt b ~pos:(base + 219) None;
    Sbe.set_u8
      b
      ~pos:(base + 221)
      (match t.ord_status with
       | Partially_filled -> 1
       | Filled -> 2);
    Sbe.set_char b ~pos:(base + 222) (Ord_type.to_char t.ord_type);
    Sbe.set_u8 b ~pos:(base + 224) (Time_in_force.to_int t.time_in_force);
    Sbe.set_bool b ~pos:(base + 227) t.aggressor;
    nulls_u8 b ~base [ 228; 231; 232; 233 ];
    let pos =
      Sbe.Group.write
        b
        ~pos:(base + block_length)
        ~block_length:Fill.block_length
        (List.length t.fills)
    in
    let pos =
      List.fold t.fills ~init:pos ~f:(fun pos fill ->
        Fill.write b ~pos fill;
        pos + Fill.block_length)
    in
    ignore (Sbe.Group.write b ~pos ~block_length:order_event_block_length 0 : int)
  ;;

  let read b ~base ~block_length =
    let ord_status : Ord_status.t =
      match Sbe.get_u8 b ~pos:(base + 221) with
      | 1 -> Partially_filled
      | 2 -> Filled
      | v -> fail "invalid trade OrdStatus %d" v
    in
    let fill_block, n, pos = Sbe.Group.read b ~pos:(base + block_length) in
    let fills = List.init n ~f:(fun i -> Fill.read b ~pos:(pos + (i * fill_block))) in
    { common = Exec_common.read layout b ~base
    ; last_px = Sbe.get_price b ~pos:(base + 100)
    ; price = Sbe.get_price b ~pos:(base + 116)
    ; sec_exec_id = Sbe.get_u64 b ~pos:(base + 156)
    ; order_qty = Sbe.get_u32 b ~pos:(base + 189)
    ; last_qty = Sbe.get_u32 b ~pos:(base + 193)
    ; cum_qty = Sbe.get_u32 b ~pos:(base + 197)
    ; md_trade_entry_id = Sbe.get_u32 b ~pos:(base + 201)
    ; side_trade_id = Sbe.get_u32 b ~pos:(base + 205)
    ; leaves_qty = Sbe.get_u32 b ~pos:(base + 213)
    ; trade_date = Sbe.get_u16_opt b ~pos:(base + 217)
    ; ord_status
    ; ord_type = decode_ord_type b ~pos:(base + 222)
    ; time_in_force = decode_tif b ~pos:(base + 224)
    ; aggressor = Sbe.get_bool b ~pos:(base + 227)
    ; fills
    }
  ;;
end

module Execution_report_modify = struct
  type t =
    { common : Exec_common.t
    ; price : Price.t
    ; stop_px : Price.t option
    ; order_qty : int
    ; cum_qty : int
    ; leaves_qty : int
    ; min_qty : int option
    ; display_qty : int option
    ; ord_type : Ord_type.t
    ; time_in_force : Time_in_force.t
    }
  [@@deriving sexp, compare, equal]

  let template_id = 531
  let block_length = 217

  let layout : Exec_common.layout =
    { exec_id_at = 12
    ; order_id_at = 100
    ; transact_time_at = 124
    ; location_at = 164
    ; security_id_at = 169
    ; side_at = 198
    ; manual_at = 200
    ; poss_retrans_at = 201
    }
  ;;

  let write b ~base t =
    Exec_common.write layout b ~base t.common;
    Sbe.set_price b ~pos:(base + 108) t.price;
    Sbe.set_price_opt b ~pos:(base + 116) t.stop_px;
    null_u64 b ~pos:(base + 148);
    null_u64 b ~pos:(base + 156);
    Sbe.set_u32 b ~pos:(base + 173) t.order_qty;
    Sbe.set_u32 b ~pos:(base + 177) t.cum_qty;
    Sbe.set_u32 b ~pos:(base + 181) t.leaves_qty;
    Sbe.set_u32_opt b ~pos:(base + 185) t.min_qty;
    Sbe.set_u32_opt b ~pos:(base + 189) t.display_qty;
    Sbe.set_u16_opt b ~pos:(base + 193) None;
    Sbe.set_u16_opt b ~pos:(base + 195) None;
    Sbe.set_char b ~pos:(base + 197) (Ord_type.to_char t.ord_type);
    Sbe.set_u8 b ~pos:(base + 199) (Time_in_force.to_int t.time_in_force);
    nulls_u8 b ~base [ 202; 203; 206; 207; 208 ];
    null_u64 b ~pos:(base + 209)
  ;;

  let read b ~base =
    { common = Exec_common.read layout b ~base
    ; price = Sbe.get_price b ~pos:(base + 108)
    ; stop_px = Sbe.get_price_opt b ~pos:(base + 116)
    ; order_qty = Sbe.get_u32 b ~pos:(base + 173)
    ; cum_qty = Sbe.get_u32 b ~pos:(base + 177)
    ; leaves_qty = Sbe.get_u32 b ~pos:(base + 181)
    ; min_qty = Sbe.get_u32_opt b ~pos:(base + 185)
    ; display_qty = Sbe.get_u32_opt b ~pos:(base + 189)
    ; ord_type = decode_ord_type b ~pos:(base + 197)
    ; time_in_force = decode_tif b ~pos:(base + 199)
    }
  ;;
end

module Execution_report_cancel = struct
  type t =
    { common : Exec_common.t
    ; price : Price.t
    ; stop_px : Price.t option
    ; order_qty : int
    ; cum_qty : int
    ; min_qty : int option
    ; display_qty : int option
    ; ord_type : Ord_type.t
    ; time_in_force : Time_in_force.t
    ; exec_restatement_reason : int option
    }
  [@@deriving sexp, compare, equal]

  let template_id = 534
  let block_length = 214

  let layout : Exec_common.layout =
    { exec_id_at = 12
    ; order_id_at = 100
    ; transact_time_at = 124
    ; location_at = 164
    ; security_id_at = 169
    ; side_at = 194
    ; manual_at = 196
    ; poss_retrans_at = 197
    }
  ;;

  let write b ~base t =
    Exec_common.write layout b ~base t.common;
    Sbe.set_price b ~pos:(base + 108) t.price;
    Sbe.set_price_opt b ~pos:(base + 116) t.stop_px;
    null_u64 b ~pos:(base + 148);
    null_u64 b ~pos:(base + 156);
    Sbe.set_u32 b ~pos:(base + 173) t.order_qty;
    Sbe.set_u32 b ~pos:(base + 177) t.cum_qty;
    Sbe.set_u32_opt b ~pos:(base + 181) t.min_qty;
    Sbe.set_u32_opt b ~pos:(base + 185) t.display_qty;
    Sbe.set_u16_opt b ~pos:(base + 189) None;
    Sbe.set_u16_opt b ~pos:(base + 191) None;
    Sbe.set_char b ~pos:(base + 193) (Ord_type.to_char t.ord_type);
    Sbe.set_u8 b ~pos:(base + 195) (Time_in_force.to_int t.time_in_force);
    set_null_u8 b ~pos:(base + 198);
    Sbe.set_u8_opt b ~pos:(base + 199) t.exec_restatement_reason;
    nulls_u8 b ~base [ 200; 203; 204; 205 ];
    null_u64 b ~pos:(base + 206)
  ;;

  let read b ~base =
    { common = Exec_common.read layout b ~base
    ; price = Sbe.get_price b ~pos:(base + 108)
    ; stop_px = Sbe.get_price_opt b ~pos:(base + 116)
    ; order_qty = Sbe.get_u32 b ~pos:(base + 173)
    ; cum_qty = Sbe.get_u32 b ~pos:(base + 177)
    ; min_qty = Sbe.get_u32_opt b ~pos:(base + 181)
    ; display_qty = Sbe.get_u32_opt b ~pos:(base + 185)
    ; ord_type = decode_ord_type b ~pos:(base + 193)
    ; time_in_force = decode_tif b ~pos:(base + 195)
    ; exec_restatement_reason = Sbe.get_u8_opt b ~pos:(base + 199)
    }
  ;;
end

module Cancel_reject = struct
  type t =
    { seq_num : int
    ; uuid : int
    ; text : string
    ; exec_id : string
    ; sender_id : string
    ; cl_ord_id : string
    ; party_details_list_req_id : int
    ; order_id : int
    ; transact_time : int
    ; sending_time : int
    ; order_request_id : int
    ; location : string
    ; cxl_rej_reason : int
    ; manual : bool
    ; poss_retrans : bool
    }
  [@@deriving sexp, compare, equal]

  let cancel_template_id = 535
  let replace_template_id = 536
  let block_length = 409

  let write b ~base t =
    Sbe.set_u32 b ~pos:base t.seq_num;
    Sbe.set_u64 b ~pos:(base + 4) t.uuid;
    Sbe.set_string b ~pos:(base + 12) ~len:256 t.text;
    Sbe.set_string b ~pos:(base + 268) ~len:40 t.exec_id;
    Sbe.set_string b ~pos:(base + 308) ~len:20 t.sender_id;
    Sbe.set_string b ~pos:(base + 328) ~len:20 t.cl_ord_id;
    Sbe.set_u64 b ~pos:(base + 348) t.party_details_list_req_id;
    Sbe.set_u64 b ~pos:(base + 356) t.order_id;
    Sbe.set_u64 b ~pos:(base + 364) t.transact_time;
    Sbe.set_u64 b ~pos:(base + 372) t.sending_time;
    Sbe.set_u64 b ~pos:(base + 380) t.order_request_id;
    Sbe.set_string b ~pos:(base + 388) ~len:5 t.location;
    Sbe.set_u16 b ~pos:(base + 393) t.cxl_rej_reason;
    Sbe.set_u16_opt b ~pos:(base + 395) None;
    Sbe.set_bool b ~pos:(base + 397) t.manual;
    Sbe.set_bool b ~pos:(base + 398) t.poss_retrans;
    nulls_u8 b ~base [ 399; 400 ];
    null_u64 b ~pos:(base + 401)
  ;;

  let read b ~base =
    { seq_num = Sbe.get_u32 b ~pos:base
    ; uuid = Sbe.get_u64 b ~pos:(base + 4)
    ; text = Sbe.get_string b ~pos:(base + 12) ~len:256
    ; exec_id = Sbe.get_string b ~pos:(base + 268) ~len:40
    ; sender_id = Sbe.get_string b ~pos:(base + 308) ~len:20
    ; cl_ord_id = Sbe.get_string b ~pos:(base + 328) ~len:20
    ; party_details_list_req_id = Sbe.get_u64 b ~pos:(base + 348)
    ; order_id = Sbe.get_u64 b ~pos:(base + 356)
    ; transact_time = Sbe.get_u64 b ~pos:(base + 364)
    ; sending_time = Sbe.get_u64 b ~pos:(base + 372)
    ; order_request_id = Sbe.get_u64 b ~pos:(base + 380)
    ; location = Sbe.get_string b ~pos:(base + 388) ~len:5
    ; cxl_rej_reason = Sbe.get_u16 b ~pos:(base + 393)
    ; manual = Sbe.get_bool b ~pos:(base + 397)
    ; poss_retrans = Sbe.get_bool b ~pos:(base + 398)
    }
  ;;
end

module Message = struct
  type t =
    | Negotiate of Negotiate.t
    | Negotiation_response of Negotiation_response.t
    | Negotiation_reject of Negotiation_reject.t
    | Establish of Establish.t
    | Establishment_ack of Establishment_ack.t
    | Establishment_reject of Establishment_reject.t
    | Sequence of Sequence.t
    | Terminate of Terminate.t
    | Retransmit_request of Retransmit_request.t
    | Retransmission of Retransmission.t
    | Retransmit_reject of Retransmit_reject.t
    | Not_applied of Not_applied.t
    | New_order_single of New_order_single.t
    | Order_cancel_replace_request of Order_cancel_replace_request.t
    | Order_cancel_request of Order_cancel_request.t
    | Business_reject of Business_reject.t
    | Execution_report_new of Execution_report_new.t
    | Execution_report_reject of Execution_report_reject.t
    | Execution_report_elimination of Execution_report_elimination.t
    | Execution_report_trade_outright of Execution_report_trade_outright.t
    | Execution_report_modify of Execution_report_modify.t
    | Execution_report_cancel of Execution_report_cancel.t
    | Order_cancel_reject of Cancel_reject.t
    | Order_cancel_replace_reject of Cancel_reject.t
  [@@deriving sexp, compare, equal]

  let template_id = function
    | Negotiate _ -> Negotiate.template_id
    | Negotiation_response _ -> Negotiation_response.template_id
    | Negotiation_reject _ -> Negotiation_reject.template_id
    | Establish _ -> Establish.template_id
    | Establishment_ack _ -> Establishment_ack.template_id
    | Establishment_reject _ -> Establishment_reject.template_id
    | Sequence _ -> Sequence.template_id
    | Terminate _ -> Terminate.template_id
    | Retransmit_request _ -> Retransmit_request.template_id
    | Retransmission _ -> Retransmission.template_id
    | Retransmit_reject _ -> Retransmit_reject.template_id
    | Not_applied _ -> Not_applied.template_id
    | New_order_single _ -> New_order_single.template_id
    | Order_cancel_replace_request _ -> Order_cancel_replace_request.template_id
    | Order_cancel_request _ -> Order_cancel_request.template_id
    | Business_reject _ -> Business_reject.template_id
    | Execution_report_new _ -> Execution_report_new.template_id
    | Execution_report_reject _ -> Execution_report_reject.template_id
    | Execution_report_elimination _ -> Execution_report_elimination.template_id
    | Execution_report_trade_outright _ -> Execution_report_trade_outright.template_id
    | Execution_report_modify _ -> Execution_report_modify.template_id
    | Execution_report_cancel _ -> Execution_report_cancel.template_id
    | Order_cancel_reject _ -> Cancel_reject.cancel_template_id
    | Order_cancel_replace_reject _ -> Cancel_reject.replace_template_id
  ;;

  let name = function
    | Negotiate _ -> "Negotiate"
    | Negotiation_response _ -> "NegotiationResponse"
    | Negotiation_reject _ -> "NegotiationReject"
    | Establish _ -> "Establish"
    | Establishment_ack _ -> "EstablishmentAck"
    | Establishment_reject _ -> "EstablishmentReject"
    | Sequence _ -> "Sequence"
    | Terminate _ -> "Terminate"
    | Retransmit_request _ -> "RetransmitRequest"
    | Retransmission _ -> "Retransmission"
    | Retransmit_reject _ -> "RetransmitReject"
    | Not_applied _ -> "NotApplied"
    | New_order_single _ -> "NewOrderSingle"
    | Order_cancel_replace_request _ -> "OrderCancelReplaceRequest"
    | Order_cancel_request _ -> "OrderCancelRequest"
    | Business_reject _ -> "BusinessReject"
    | Execution_report_new _ -> "ExecutionReportNew"
    | Execution_report_reject _ -> "ExecutionReportReject"
    | Execution_report_elimination _ -> "ExecutionReportElimination"
    | Execution_report_trade_outright _ -> "ExecutionReportTradeOutright"
    | Execution_report_modify _ -> "ExecutionReportModify"
    | Execution_report_cancel _ -> "ExecutionReportCancel"
    | Order_cancel_reject _ -> "OrderCancelReject"
    | Order_cancel_replace_reject _ -> "OrderCancelReplaceReject"
  ;;

  let seq_num = function
    | New_order_single t -> Some t.seq_num
    | Order_cancel_replace_request t -> Some t.seq_num
    | Order_cancel_request t -> Some t.seq_num
    | Business_reject t -> Some t.seq_num
    | Execution_report_new t -> Some t.common.seq_num
    | Execution_report_reject t -> Some t.common.seq_num
    | Execution_report_elimination t -> Some t.common.seq_num
    | Execution_report_trade_outright t -> Some t.common.seq_num
    | Execution_report_modify t -> Some t.common.seq_num
    | Execution_report_cancel t -> Some t.common.seq_num
    | Order_cancel_reject t | Order_cancel_replace_reject t -> Some t.seq_num
    | Negotiate _
    | Negotiation_response _
    | Negotiation_reject _
    | Establish _
    | Establishment_ack _
    | Establishment_reject _
    | Sequence _
    | Terminate _
    | Retransmit_request _
    | Retransmission _
    | Retransmit_reject _
    | Not_applied _ -> None
  ;;

  let map_common t ~f =
    match t with
    | Execution_report_new r -> Execution_report_new { r with common = f r.common }
    | Execution_report_reject r -> Execution_report_reject { r with common = f r.common }
    | Execution_report_elimination r ->
      Execution_report_elimination { r with common = f r.common }
    | Execution_report_trade_outright r ->
      Execution_report_trade_outright { r with common = f r.common }
    | Execution_report_modify r -> Execution_report_modify { r with common = f r.common }
    | Execution_report_cancel r -> Execution_report_cancel { r with common = f r.common }
    | other -> other
  ;;

  let with_sequence t ~seq_num ~uuid =
    match t with
    | Business_reject r -> Business_reject { r with seq_num; uuid }
    | Order_cancel_reject r -> Order_cancel_reject { r with seq_num; uuid }
    | Order_cancel_replace_reject r ->
      Order_cancel_replace_reject { r with seq_num; uuid }
    | other -> map_common other ~f:(fun c -> { c with seq_num; uuid })
  ;;

  let as_retransmission t =
    match t with
    | Business_reject r -> Business_reject { r with poss_retrans = true }
    | Order_cancel_reject r -> Order_cancel_reject { r with poss_retrans = true }
    | Order_cancel_replace_reject r ->
      Order_cancel_replace_reject { r with poss_retrans = true }
    | other -> map_common other ~f:(fun c -> { c with poss_retrans = true })
  ;;
end

let frame_prefix = Sbe.Sofh.size + Sbe.Header.size

(* [extra] bytes follow the root block (groups, variable-length data). *)
let frame ~template_id ~block_length ?(extra = 0) write =
  let size = frame_prefix + block_length + extra in
  let b = Bigstring.init size ~f:(fun _ -> '\000') in
  Sbe.Sofh.write b ~pos:0 ~message_size:size;
  Sbe.Header.write
    b
    ~pos:Sbe.Sofh.size
    { block_length; template_id; schema_id; version = schema_version };
  write b ~base:frame_prefix;
  b
;;

(* Negotiate, NegotiationResponse and Establish end with an empty [Credentials]
   variable-length field: a u16 length of zero. *)
let empty_credentials = 2

let encode (m : Message.t) =
  match m with
  | Negotiate t ->
    frame
      ~template_id:Negotiate.template_id
      ~block_length:Negotiate.block_length
      ~extra:empty_credentials
      (fun b ~base -> Negotiate.write b ~base t)
  | Negotiation_response t ->
    frame
      ~template_id:Negotiation_response.template_id
      ~block_length:Negotiation_response.block_length
      ~extra:empty_credentials
      (fun b ~base -> Negotiation_response.write b ~base t)
  | Negotiation_reject t ->
    frame
      ~template_id:Negotiation_reject.template_id
      ~block_length:Negotiation_reject.block_length
      (fun b ~base -> Negotiation_reject.write b ~base t)
  | Establish t ->
    frame
      ~template_id:Establish.template_id
      ~block_length:Establish.block_length
      ~extra:empty_credentials
      (fun b ~base -> Establish.write b ~base t)
  | Establishment_ack t ->
    frame
      ~template_id:Establishment_ack.template_id
      ~block_length:Establishment_ack.block_length
      (fun b ~base -> Establishment_ack.write b ~base t)
  | Establishment_reject t ->
    frame
      ~template_id:Establishment_reject.template_id
      ~block_length:Establishment_reject.block_length
      (fun b ~base -> Establishment_reject.write b ~base t)
  | Sequence t ->
    frame
      ~template_id:Sequence.template_id
      ~block_length:Sequence.block_length
      (fun b ~base -> Sequence.write b ~base t)
  | Terminate t ->
    frame
      ~template_id:Terminate.template_id
      ~block_length:Terminate.block_length
      (fun b ~base -> Terminate.write b ~base t)
  | Retransmit_request t ->
    frame
      ~template_id:Retransmit_request.template_id
      ~block_length:Retransmit_request.block_length
      (fun b ~base -> Retransmit_request.write b ~base t)
  | Retransmission t ->
    frame
      ~template_id:Retransmission.template_id
      ~block_length:Retransmission.block_length
      (fun b ~base -> Retransmission.write b ~base t)
  | Retransmit_reject t ->
    frame
      ~template_id:Retransmit_reject.template_id
      ~block_length:Retransmit_reject.block_length
      (fun b ~base -> Retransmit_reject.write b ~base t)
  | Not_applied t ->
    frame
      ~template_id:Not_applied.template_id
      ~block_length:Not_applied.block_length
      (fun b ~base -> Not_applied.write b ~base t)
  | New_order_single t ->
    frame
      ~template_id:New_order_single.template_id
      ~block_length:New_order_single.block_length
      (fun b ~base -> New_order_single.write b ~base t)
  | Order_cancel_replace_request t ->
    frame
      ~template_id:Order_cancel_replace_request.template_id
      ~block_length:Order_cancel_replace_request.block_length
      (fun b ~base -> Order_cancel_replace_request.write b ~base t)
  | Order_cancel_request t ->
    frame
      ~template_id:Order_cancel_request.template_id
      ~block_length:Order_cancel_request.block_length
      (fun b ~base -> Order_cancel_request.write b ~base t)
  | Business_reject t ->
    frame
      ~template_id:Business_reject.template_id
      ~block_length:Business_reject.block_length
      (fun b ~base -> Business_reject.write b ~base t)
  | Execution_report_new t ->
    frame
      ~template_id:Execution_report_new.template_id
      ~block_length:Execution_report_new.block_length
      (fun b ~base -> Execution_report_new.write b ~base t)
  | Execution_report_reject t ->
    frame
      ~template_id:Execution_report_reject.template_id
      ~block_length:Execution_report_reject.block_length
      (fun b ~base -> Execution_report_reject.write b ~base t)
  | Execution_report_elimination t ->
    frame
      ~template_id:Execution_report_elimination.template_id
      ~block_length:Execution_report_elimination.block_length
      (fun b ~base -> Execution_report_elimination.write b ~base t)
  | Execution_report_trade_outright t ->
    frame
      ~template_id:Execution_report_trade_outright.template_id
      ~block_length:Execution_report_trade_outright.block_length
      ~extra:(Execution_report_trade_outright.extra t)
      (fun b ~base -> Execution_report_trade_outright.write b ~base t)
  | Execution_report_modify t ->
    frame
      ~template_id:Execution_report_modify.template_id
      ~block_length:Execution_report_modify.block_length
      (fun b ~base -> Execution_report_modify.write b ~base t)
  | Execution_report_cancel t ->
    frame
      ~template_id:Execution_report_cancel.template_id
      ~block_length:Execution_report_cancel.block_length
      (fun b ~base -> Execution_report_cancel.write b ~base t)
  | Order_cancel_reject t ->
    frame
      ~template_id:Cancel_reject.cancel_template_id
      ~block_length:Cancel_reject.block_length
      (fun b ~base -> Cancel_reject.write b ~base t)
  | Order_cancel_replace_reject t ->
    frame
      ~template_id:Cancel_reject.replace_template_id
      ~block_length:Cancel_reject.block_length
      (fun b ~base -> Cancel_reject.write b ~base t)
;;

let decode_exn b =
  let len = Bigstring.length b in
  if len < frame_prefix then fail "frame shorter than SOFH and header (%d bytes)" len;
  let size = Or_error.ok_exn (Sbe.Sofh.read b ~pos:0) in
  if size > len then fail "SOFH size %d exceeds the %d bytes received" size len;
  let header = Sbe.Header.read b ~pos:Sbe.Sofh.size in
  if header.schema_id <> schema_id then fail "unexpected schema id %d" header.schema_id;
  let need block_length =
    if header.block_length < block_length || frame_prefix + header.block_length > size
    then
      fail
        "template %d: block length %d, expected at least %d"
        header.template_id
        header.block_length
        block_length
  in
  let base = frame_prefix in
  let (m : Message.t) =
    match header.template_id with
    | 500 ->
      need Negotiate.block_length;
      Negotiate (Negotiate.read b ~base)
    | 501 ->
      need Negotiation_response.block_length;
      Negotiation_response (Negotiation_response.read b ~base)
    | 502 ->
      need Negotiation_reject.block_length;
      Negotiation_reject (Negotiation_reject.read b ~base)
    | 503 ->
      need Establish.block_length;
      Establish (Establish.read b ~base)
    | 504 ->
      need Establishment_ack.block_length;
      Establishment_ack (Establishment_ack.read b ~base)
    | 505 ->
      need Establishment_reject.block_length;
      Establishment_reject (Establishment_reject.read b ~base)
    | 506 ->
      need Sequence.block_length;
      Sequence (Sequence.read b ~base)
    | 507 ->
      need Terminate.block_length;
      Terminate (Terminate.read b ~base)
    | 508 ->
      need Retransmit_request.block_length;
      Retransmit_request (Retransmit_request.read b ~base)
    | 509 ->
      need Retransmission.block_length;
      Retransmission (Retransmission.read b ~base)
    | 510 ->
      need Retransmit_reject.block_length;
      Retransmit_reject (Retransmit_reject.read b ~base)
    | 513 ->
      need Not_applied.block_length;
      Not_applied (Not_applied.read b ~base)
    | 514 ->
      need New_order_single.block_length;
      New_order_single (New_order_single.read b ~base)
    | 515 ->
      need Order_cancel_replace_request.block_length;
      Order_cancel_replace_request (Order_cancel_replace_request.read b ~base)
    | 516 ->
      need Order_cancel_request.block_length;
      Order_cancel_request (Order_cancel_request.read b ~base)
    | 521 ->
      need Business_reject.block_length;
      Business_reject (Business_reject.read b ~base)
    | 522 ->
      need Execution_report_new.block_length;
      Execution_report_new (Execution_report_new.read b ~base)
    | 523 ->
      need Execution_report_reject.block_length;
      Execution_report_reject (Execution_report_reject.read b ~base)
    | 524 ->
      need Execution_report_elimination.block_length;
      Execution_report_elimination (Execution_report_elimination.read b ~base)
    | 525 ->
      need Execution_report_trade_outright.block_length;
      Execution_report_trade_outright
        (Execution_report_trade_outright.read b ~base ~block_length:header.block_length)
    | 531 ->
      need Execution_report_modify.block_length;
      Execution_report_modify (Execution_report_modify.read b ~base)
    | 534 ->
      need Execution_report_cancel.block_length;
      Execution_report_cancel (Execution_report_cancel.read b ~base)
    | 535 ->
      need Cancel_reject.block_length;
      Order_cancel_reject (Cancel_reject.read b ~base)
    | 536 ->
      need Cancel_reject.block_length;
      Order_cancel_replace_reject (Cancel_reject.read b ~base)
    | id -> fail "unknown iLink 3 template %d" id
  in
  m
;;

let decode b =
  match decode_exn b with
  | m -> Ok m
  | exception Decode_error s -> Or_error.error_string s
  | exception exn -> Or_error.of_exn exn
;;
