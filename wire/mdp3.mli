(** MDP 3.0 market data (schema id 1, version 13): the packet header and the in-scope
    templates.

    A packet is [MsgSeqNum u32] and [SendingTime u64] followed by messages, each message a
    [MsgSize u16] (the message including this field), the 8-byte SBE header and the body.
    Over UDP one datagram is one packet. The simulator carries packets over TCP and puts a
    4-byte SOFH ([MessageSize u16] including the SOFH, [0xCAFE]) in front of each packet
    so a reader can split the stream; see {!encode_frame}. *)

open! Core
module Price := Exchange_core.Price

val schema_id : int
val schema_version : int

(** [MatchEventIndicator] bits. *)
module Match_event : sig
  type t = int [@@deriving sexp, compare, equal]

  val last_trade_msg : t
  val last_volume_msg : t
  val last_quote_msg : t
  val last_stats_msg : t
  val last_implied_msg : t
  val recovery_msg : t
  val end_of_event : t

  (** Names of the bits set, e.g. [["LastQuoteMsg"; "EndOfEvent"]]. *)
  val to_names : t -> string list
end

module Trading_status : sig
  type t =
    | Trading_halt (** 2 *)
    | Close (** 4 *)
    | New_price_indication (** 15 *)
    | Ready_to_trade (** 17, open *)
    | Not_available_for_trading (** 18 *)
    | Unknown_or_invalid (** 20 *)
    | Pre_open (** 21 *)
    | Pre_cross (** 24 *)
    | Cross (** 25 *)
    | Post_close (** 26 *)
    | No_change (** 103 *)
  [@@deriving sexp, compare, equal, enumerate]

  val to_int : t -> int
  val of_int : int -> t option
end

module Update_action : sig
  type t =
    | New
    | Change
    | Delete
    | Delete_thru
    | Delete_from
    | Overlay
  [@@deriving sexp, compare, equal, enumerate]

  val to_int : t -> int
  val of_int : int -> t option
end

module Channel_reset : sig
  type t =
    { transact_time : int
    ; match_event_indicator : Match_event.t
    ; appl_ids : int list
    }
  [@@deriving sexp, compare, equal]
end

module Security_status : sig
  type t =
    { transact_time : int
    ; security_group : string (** 6 bytes *)
    ; asset : string (** 6 bytes *)
    ; security_id : int option (** [None]: the whole group *)
    ; trade_date : int option (** days since the Unix epoch *)
    ; match_event_indicator : Match_event.t
    ; trading_status : Trading_status.t option
    ; halt_reason : int (** 0 group schedule, 1 surveillance, 2 market event, ... *)
    ; trading_event : int (** 0 no event, 1 no cancel, 4 reset statistics, ... *)
    }
  [@@deriving sexp, compare, equal]
end

module Instrument_definition_future : sig
  module Event : sig
    type t =
      { event_type : int (** 5 activation, 7 last eligible trade date *)
      ; event_time : int
      }
    [@@deriving sexp, compare, equal]
  end

  module Feed_type : sig
    type t =
      { md_feed_type : string (** ["GBX"] outright book, ["GBI"] implied *)
      ; market_depth : int
      }
    [@@deriving sexp, compare, equal]
  end

  module Lot_type_rule : sig
    type t =
      { lot_type : int
      ; min_lot_size : int (** [DecimalQty] mantissa, exponent -4 *)
      }
    [@@deriving sexp, compare, equal]
  end

  type t =
    { match_event_indicator : Match_event.t
    ; tot_num_reports : int option
    ; security_update_action : char (** ['A'] add, ['D'] delete, ['M'] modify *)
    ; last_update_time : int
    ; trading_status : Trading_status.t option
    ; appl_id : int
    ; market_segment_id : int
    ; underlying_product : int
    ; security_exchange : string (** 4 bytes, e.g. ["XCME"] *)
    ; security_group : string
    ; asset : string
    ; symbol : string (** 20 bytes, e.g. ["ESZ6"] *)
    ; security_id : int
    ; security_type : string (** ["FUT"] *)
    ; cfi_code : string (** ["FFIXSX"] *)
    ; maturity_year : int
    ; maturity_month : int
    ; currency : string
    ; settl_currency : string
    ; match_algorithm : char (** ['F'] FIFO *)
    ; min_trade_vol : int
    ; max_trade_vol : int
    ; min_price_increment : Price.t
    ; display_factor : Price.t (** [Decimal9] *)
    ; unit_of_measure : string (** 30 bytes *)
    ; unit_of_measure_qty : Price.t option (** [Decimal9NULL] *)
    ; trading_reference_price : Price.t option
    ; settl_price_type : int
    ; open_interest_qty : int option
    ; cleared_volume : int option
    ; high_limit_price : Price.t option
    ; low_limit_price : Price.t option
    ; max_price_variation : Price.t option
    ; contract_multiplier : int option
    ; min_price_increment_amount : Price.t option
    ; trading_reference_date : int option
    ; events : Event.t list
    ; feed_types : Feed_type.t list
    ; inst_attrib_value : int option (** one [NoInstAttrib] entry when set *)
    ; lot_type_rules : Lot_type_rule.t list
    }
  [@@deriving sexp, compare, equal]
end

module Message : sig
  type t =
    | Channel_reset of Channel_reset.t
    | Admin_heartbeat
    | Security_status of Security_status.t
    | Instrument_definition_future of Instrument_definition_future.t
  [@@deriving sexp, compare, equal]

  val template_id : t -> int
  val name : t -> string
end

module Packet : sig
  type t =
    { msg_seq_num : int
    ; sending_time : int
    ; messages : Message.t list
    }
  [@@deriving sexp, compare, equal]

  (** [MsgSeqNum] and [SendingTime]. *)
  val header_size : int
end

(** [MsgSize], SBE header and body of one message. *)
val encode_message : Message.t -> Bigstring.t

(** Decodes the message at [pos]; returns it with the position after it. Unknown templates
    are skipped and reported as [None]. *)
val decode_message : Bigstring.t -> pos:int -> (Message.t option * int) Or_error.t

(** A packet as sent in one UDP datagram. *)
val encode_packet : Packet.t -> Bigstring.t

val decode_packet : Bigstring.t -> Packet.t Or_error.t

(** A packet prefixed with a SOFH, as the simulator sends it over TCP. *)
val encode_frame : Packet.t -> Bigstring.t

(** Decodes a SOFH-prefixed packet. *)
val decode_frame : Bigstring.t -> Packet.t Or_error.t
