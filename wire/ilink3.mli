(** iLink 3 order entry messages (schema id 8, version 5): the FIXP session layer and the
    in-scope application messages.

    A frame on the wire is a 4-byte SOFH, the 8-byte SBE message header and the root
    block, followed by repeating groups or variable-length data where the template has
    them. Encoders write every field of the root block at the offsets of the published
    schema, fields this codec does not model are written as their null value. Decoders
    read the root block by offset and use the header's [BlockLength] to find what follows
    it, so newer schema versions that append fields still decode. *)

open! Core
module Price := Exchange_core.Price

val schema_id : int
val schema_version : int

module Side : sig
  type t =
    | Buy
    | Sell
  [@@deriving sexp, compare, equal, enumerate]

  val to_int : t -> int
  val of_int : int -> t option
end

module Ord_type : sig
  type t =
    | Market_with_protection (** ['1'] *)
    | Limit (** ['2'] *)
    | Stop_with_protection (** ['3'] *)
    | Stop_limit (** ['4'] *)
    | Market_with_leftover_as_limit (** ['K'] *)
  [@@deriving sexp, compare, equal, enumerate]

  val to_char : t -> char
  val of_char : char -> t option
end

module Time_in_force : sig
  type t =
    | Day (** 0 *)
    | Gtc (** 1 *)
    | Fak (** 3, fill and kill *)
    | Fok (** 4, fill or kill *)
    | Gtd (** 6 *)
  [@@deriving sexp, compare, equal, enumerate]

  val to_int : t -> int
  val of_int : int -> t option
end

(** {2 Session layer (FIXP)} *)

module Negotiate : sig
  type t =
    { hmac_signature : string (** 32 bytes *)
    ; access_key_id : string (** 20 bytes *)
    ; uuid : int
    ; request_timestamp : int
    ; session : string (** 3 bytes *)
    ; firm : string (** 5 bytes *)
    }
  [@@deriving sexp, compare, equal]
end

module Negotiation_response : sig
  type t =
    { uuid : int
    ; request_timestamp : int
    ; secret_key_secure_id_expiration : int option
    ; previous_seq_no : int
    ; previous_uuid : int
    }
  [@@deriving sexp, compare, equal]
end

module Negotiation_reject : sig
  type t =
    { reason : string
    ; uuid : int
    ; request_timestamp : int
    ; error_codes : int
    }
  [@@deriving sexp, compare, equal]
end

module Establish : sig
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
    ; keep_alive_interval : int (** milliseconds *)
    }
  [@@deriving sexp, compare, equal]
end

module Establishment_ack : sig
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
end

module Establishment_reject : sig
  type t =
    { reason : string
    ; uuid : int
    ; request_timestamp : int
    ; next_seq_no : int
    ; error_codes : int
    }
  [@@deriving sexp, compare, equal]
end

module Sequence : sig
  type t =
    { uuid : int
    ; next_seq_no : int
    ; keep_alive_interval_lapsed : bool
    }
  [@@deriving sexp, compare, equal]
end

module Terminate : sig
  type t =
    { reason : string
    ; uuid : int
    ; request_timestamp : int
    ; error_codes : int
    }
  [@@deriving sexp, compare, equal]
end

module Retransmit_request : sig
  type t =
    { uuid : int
    ; last_uuid : int option
    ; request_timestamp : int
    ; from_seq_no : int
    ; msg_count : int
    }
  [@@deriving sexp, compare, equal]
end

module Retransmission : sig
  type t =
    { uuid : int
    ; last_uuid : int option
    ; request_timestamp : int
    ; from_seq_no : int
    ; msg_count : int
    }
  [@@deriving sexp, compare, equal]
end

module Retransmit_reject : sig
  type t =
    { reason : string
    ; uuid : int
    ; last_uuid : int option
    ; request_timestamp : int
    ; error_codes : int
    }
  [@@deriving sexp, compare, equal]
end

module Not_applied : sig
  type t =
    { uuid : int
    ; from_seq_no : int
    ; msg_count : int
    }
  [@@deriving sexp, compare, equal]
end

(** {2 Client requests} *)

module New_order_single : sig
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
end

module Order_cancel_replace_request : sig
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
end

module Order_cancel_request : sig
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
end

(** {2 Exchange responses} *)

(** Fields shared by the execution reports. *)
module Exec_common : sig
  type t =
    { seq_num : int
    ; uuid : int
    ; exec_id : string (** 40 bytes *)
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
end

module Business_reject : sig
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
    ; ref_msg_type : string (** 2 bytes *)
    ; poss_retrans : bool
    }
  [@@deriving sexp, compare, equal]
end

module Execution_report_new : sig
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
end

module Execution_report_reject : sig
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
end

module Execution_report_elimination : sig
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
end

module Fill : sig
  type t =
    { fill_px : Price.t
    ; fill_qty : int
    ; fill_exec_id : string (** 2 bytes *)
    ; fill_yield_type : int
    }
  [@@deriving sexp, compare, equal]
end

module Execution_report_trade_outright : sig
  module Ord_status : sig
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
    ; md_trade_entry_id : int (** links to the MDP TradeSummary entry *)
    ; side_trade_id : int
    ; leaves_qty : int
    ; trade_date : int option (** days since the Unix epoch *)
    ; ord_status : Ord_status.t
    ; ord_type : Ord_type.t
    ; time_in_force : Time_in_force.t
    ; aggressor : bool
    ; fills : Fill.t list
    }
  [@@deriving sexp, compare, equal]
end

module Execution_report_modify : sig
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
end

module Execution_report_cancel : sig
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
    ; exec_restatement_reason : int option (** e.g. 100 cancel on disconnect *)
    }
  [@@deriving sexp, compare, equal]
end

(** OrderCancelReject (535) and OrderCancelReplaceReject (536) share this layout. *)
module Cancel_reject : sig
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
end

(** {2 Messages} *)

module Message : sig
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

  val template_id : t -> int

  (** Template name without the id, e.g. ["NewOrderSingle"]. *)
  val name : t -> string

  (** Business (application) messages carry a [SeqNum]; session messages do not. *)
  val seq_num : t -> int option

  (** Sets [SeqNum] and [UUID] of an exchange business message (identity otherwise). *)
  val with_sequence : t -> seq_num:int -> uuid:int -> t

  (** Sets [PossRetransFlag] on an exchange business message, for replays. *)
  val as_retransmission : t -> t
end

(** The whole frame: SOFH, header, body. *)
val encode : Message.t -> Bigstring.t

(** Decodes one whole frame (as produced by {!encode}). Unknown templates and a wrong
    schema id are errors. *)
val decode : Bigstring.t -> Message.t Or_error.t
