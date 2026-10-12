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
  ;;

  let seq_num = function
    | New_order_single t -> Some t.seq_num
    | Order_cancel_replace_request t -> Some t.seq_num
    | Order_cancel_request t -> Some t.seq_num
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
