open! Core
module Price = Exchange_core.Price

let schema_id = 1
let schema_version = 13

module Match_event = struct
  type t = int [@@deriving sexp, compare, equal]

  let last_trade_msg = 0x01
  let last_volume_msg = 0x02
  let last_quote_msg = 0x04
  let last_stats_msg = 0x08
  let last_implied_msg = 0x10
  let recovery_msg = 0x20
  let end_of_event = 0x80

  let names =
    [ last_trade_msg, "LastTradeMsg"
    ; last_volume_msg, "LastVolumeMsg"
    ; last_quote_msg, "LastQuoteMsg"
    ; last_stats_msg, "LastStatsMsg"
    ; last_implied_msg, "LastImpliedMsg"
    ; recovery_msg, "RecoveryMsg"
    ; end_of_event, "EndOfEvent"
    ]
  ;;

  let to_names t =
    List.filter_map names ~f:(fun (bit, n) -> Option.some_if (t land bit <> 0) n)
  ;;
end

module Trading_status = struct
  type t =
    | Trading_halt
    | Close
    | New_price_indication
    | Ready_to_trade
    | Not_available_for_trading
    | Unknown_or_invalid
    | Pre_open
    | Pre_cross
    | Cross
    | Post_close
    | No_change
  [@@deriving sexp, compare, equal, enumerate]

  let to_int = function
    | Trading_halt -> 2
    | Close -> 4
    | New_price_indication -> 15
    | Ready_to_trade -> 17
    | Not_available_for_trading -> 18
    | Unknown_or_invalid -> 20
    | Pre_open -> 21
    | Pre_cross -> 24
    | Cross -> 25
    | Post_close -> 26
    | No_change -> 103
  ;;

  let of_int i = List.find all ~f:(fun t -> to_int t = i)
end

module Update_action = struct
  type t =
    | New
    | Change
    | Delete
    | Delete_thru
    | Delete_from
    | Overlay
  [@@deriving sexp, compare, equal, enumerate]

  let to_int = function
    | New -> 0
    | Change -> 1
    | Delete -> 2
    | Delete_thru -> 3
    | Delete_from -> 4
    | Overlay -> 5
  ;;

  let of_int i = List.find all ~f:(fun t -> to_int t = i)
end

exception Decode_error of string

let fail fmt = ksprintf (fun s -> raise (Decode_error s)) fmt

let get_status b ~pos =
  match Sbe.get_u8_opt b ~pos with
  | None -> None
  | Some v ->
    (match Trading_status.of_int v with
     | Some s -> Some s
     | None -> fail "invalid SecurityTradingStatus %d" v)
;;

let set_status b ~pos s = Sbe.set_u8_opt b ~pos (Option.map s ~f:Trading_status.to_int)

(* Reads a repeating group: [f pos] decodes the entry at [pos]. Returns the entries and
   the position after the group. *)
let read_group ?eight_byte b ~pos ~f =
  let block_length, n, first = Sbe.Group.read ?eight_byte b ~pos in
  List.init n ~f:(fun i -> f (first + (i * block_length))), first + (n * block_length)
;;

let write_group ?eight_byte b ~pos ~block_length entries ~f =
  let first = Sbe.Group.write ?eight_byte b ~pos ~block_length (List.length entries) in
  List.iteri entries ~f:(fun i e -> f (first + (i * block_length)) e);
  first + (List.length entries * block_length)
;;

let group_size ?(eight_byte = false) ~block_length entries =
  (if eight_byte then Sbe.Group.size_8byte else Sbe.Group.size)
  + (block_length * List.length entries)
;;

module Channel_reset = struct
  type t =
    { transact_time : int
    ; match_event_indicator : Match_event.t
    ; appl_ids : int list
    }
  [@@deriving sexp, compare, equal]

  let template_id = 4
  let block_length = 9
  let entry_length = 2
  let groups_size t = group_size ~block_length:entry_length t.appl_ids

  let write b ~base t =
    Sbe.set_u64 b ~pos:base t.transact_time;
    Sbe.set_u8 b ~pos:(base + 8) t.match_event_indicator;
    ignore
      (write_group
         b
         ~pos:(base + block_length)
         ~block_length:entry_length
         t.appl_ids
         ~f:(fun pos id -> Sbe.set_i16 b ~pos id)
       : int)
  ;;

  let read b ~base ~block_length =
    let appl_ids, _ =
      read_group b ~pos:(base + block_length) ~f:(fun pos -> Sbe.get_i16 b ~pos)
    in
    { transact_time = Sbe.get_u64 b ~pos:base
    ; match_event_indicator = Sbe.get_u8 b ~pos:(base + 8)
    ; appl_ids
    }
  ;;
end

module Security_status = struct
  type t =
    { transact_time : int
    ; security_group : string
    ; asset : string
    ; security_id : int option
    ; trade_date : int option
    ; match_event_indicator : Match_event.t
    ; trading_status : Trading_status.t option
    ; halt_reason : int
    ; trading_event : int
    }
  [@@deriving sexp, compare, equal]

  let template_id = 30
  let block_length = 30

  let write b ~base t =
    Sbe.set_u64 b ~pos:base t.transact_time;
    Sbe.set_string b ~pos:(base + 8) ~len:6 t.security_group;
    Sbe.set_string b ~pos:(base + 14) ~len:6 t.asset;
    Sbe.set_i32_opt b ~pos:(base + 20) t.security_id;
    Sbe.set_u16_opt b ~pos:(base + 24) t.trade_date;
    Sbe.set_u8 b ~pos:(base + 26) t.match_event_indicator;
    set_status b ~pos:(base + 27) t.trading_status;
    Sbe.set_u8 b ~pos:(base + 28) t.halt_reason;
    Sbe.set_u8 b ~pos:(base + 29) t.trading_event
  ;;

  let read b ~base =
    { transact_time = Sbe.get_u64 b ~pos:base
    ; security_group = Sbe.get_string b ~pos:(base + 8) ~len:6
    ; asset = Sbe.get_string b ~pos:(base + 14) ~len:6
    ; security_id = Sbe.get_i32_opt b ~pos:(base + 20)
    ; trade_date = Sbe.get_u16_opt b ~pos:(base + 24)
    ; match_event_indicator = Sbe.get_u8 b ~pos:(base + 26)
    ; trading_status = get_status b ~pos:(base + 27)
    ; halt_reason = Sbe.get_u8 b ~pos:(base + 28)
    ; trading_event = Sbe.get_u8 b ~pos:(base + 29)
    }
  ;;
end

module Instrument_definition_future = struct
  module Event = struct
    type t =
      { event_type : int
      ; event_time : int
      }
    [@@deriving sexp, compare, equal]

    let block_length = 9
  end

  module Feed_type = struct
    type t =
      { md_feed_type : string
      ; market_depth : int
      }
    [@@deriving sexp, compare, equal]

    let block_length = 4
  end

  module Lot_type_rule = struct
    type t =
      { lot_type : int
      ; min_lot_size : int
      }
    [@@deriving sexp, compare, equal]

    let block_length = 5
  end

  type t =
    { match_event_indicator : Match_event.t
    ; tot_num_reports : int option
    ; security_update_action : char
    ; last_update_time : int
    ; trading_status : Trading_status.t option
    ; appl_id : int
    ; market_segment_id : int
    ; underlying_product : int
    ; security_exchange : string
    ; security_group : string
    ; asset : string
    ; symbol : string
    ; security_id : int
    ; security_type : string
    ; cfi_code : string
    ; maturity_year : int
    ; maturity_month : int
    ; currency : string
    ; settl_currency : string
    ; match_algorithm : char
    ; min_trade_vol : int
    ; max_trade_vol : int
    ; min_price_increment : Price.t
    ; display_factor : Price.t
    ; unit_of_measure : string
    ; unit_of_measure_qty : Price.t option
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
    ; inst_attrib_value : int option
    ; lot_type_rules : Lot_type_rule.t list
    }
  [@@deriving sexp, compare, equal]

  let template_id = 54
  let block_length = 224
  let inst_attrib_block_length = 4

  let groups_size t =
    group_size ~block_length:Event.block_length t.events
    + group_size ~block_length:Feed_type.block_length t.feed_types
    + group_size
        ~block_length:inst_attrib_block_length
        (Option.to_list t.inst_attrib_value)
    + group_size ~block_length:Lot_type_rule.block_length t.lot_type_rules
  ;;

  let write b ~base t =
    let at o = base + o in
    Sbe.set_u8 b ~pos:base t.match_event_indicator;
    Sbe.set_u32_opt b ~pos:(at 1) t.tot_num_reports;
    Sbe.set_char b ~pos:(at 5) t.security_update_action;
    Sbe.set_u64 b ~pos:(at 6) t.last_update_time;
    set_status b ~pos:(at 14) t.trading_status;
    Sbe.set_i16 b ~pos:(at 15) t.appl_id;
    Sbe.set_u8 b ~pos:(at 17) t.market_segment_id;
    Sbe.set_u8 b ~pos:(at 18) t.underlying_product;
    Sbe.set_string b ~pos:(at 19) ~len:4 t.security_exchange;
    Sbe.set_string b ~pos:(at 23) ~len:6 t.security_group;
    Sbe.set_string b ~pos:(at 29) ~len:6 t.asset;
    Sbe.set_string b ~pos:(at 35) ~len:20 t.symbol;
    Sbe.set_i32 b ~pos:(at 55) t.security_id;
    Sbe.set_string b ~pos:(at 59) ~len:6 t.security_type;
    Sbe.set_string b ~pos:(at 65) ~len:6 t.cfi_code;
    Sbe.set_u16 b ~pos:(at 71) t.maturity_year;
    Sbe.set_u8 b ~pos:(at 73) t.maturity_month;
    Sbe.set_u8_opt b ~pos:(at 74) None;
    Sbe.set_u8_opt b ~pos:(at 75) None;
    Sbe.set_string b ~pos:(at 76) ~len:3 t.currency;
    Sbe.set_string b ~pos:(at 79) ~len:3 t.settl_currency;
    Sbe.set_char b ~pos:(at 82) t.match_algorithm;
    Sbe.set_u32 b ~pos:(at 83) t.min_trade_vol;
    Sbe.set_u32 b ~pos:(at 87) t.max_trade_vol;
    Sbe.set_price b ~pos:(at 91) t.min_price_increment;
    Sbe.set_price b ~pos:(at 99) t.display_factor;
    Sbe.set_u8_opt b ~pos:(at 107) None;
    Sbe.set_u8_opt b ~pos:(at 108) None;
    Sbe.set_u8_opt b ~pos:(at 109) None;
    Sbe.set_string b ~pos:(at 110) ~len:30 t.unit_of_measure;
    Sbe.set_price_opt b ~pos:(at 140) t.unit_of_measure_qty;
    Sbe.set_price_opt b ~pos:(at 148) t.trading_reference_price;
    Sbe.set_u8 b ~pos:(at 156) t.settl_price_type;
    Sbe.set_i32_opt b ~pos:(at 157) t.open_interest_qty;
    Sbe.set_i32_opt b ~pos:(at 161) t.cleared_volume;
    Sbe.set_price_opt b ~pos:(at 165) t.high_limit_price;
    Sbe.set_price_opt b ~pos:(at 173) t.low_limit_price;
    Sbe.set_price_opt b ~pos:(at 181) t.max_price_variation;
    Sbe.set_i32_opt b ~pos:(at 189) None;
    Sbe.set_u16_opt b ~pos:(at 193) None;
    Sbe.set_i32_opt b ~pos:(at 195) None;
    Sbe.set_i32_opt b ~pos:(at 199) t.contract_multiplier;
    Sbe.set_i8 b ~pos:(at 203) 127;
    Sbe.set_i8 b ~pos:(at 204) 127;
    Sbe.set_price_opt b ~pos:(at 205) t.min_price_increment_amount;
    Sbe.set_char b ~pos:(at 213) 'N';
    Sbe.set_u16_opt b ~pos:(at 214) t.trading_reference_date;
    Sbe.set_u64_opt b ~pos:(at 216) None;
    let pos =
      write_group
        b
        ~pos:(at block_length)
        ~block_length:Event.block_length
        t.events
        ~f:(fun pos (e : Event.t) ->
          Sbe.set_u8 b ~pos e.event_type;
          Sbe.set_u64 b ~pos:(pos + 1) e.event_time)
    in
    let pos =
      write_group
        b
        ~pos
        ~block_length:Feed_type.block_length
        t.feed_types
        ~f:(fun pos (f : Feed_type.t) ->
          Sbe.set_string b ~pos ~len:3 f.md_feed_type;
          Sbe.set_i8 b ~pos:(pos + 3) f.market_depth)
    in
    let pos =
      write_group
        b
        ~pos
        ~block_length:inst_attrib_block_length
        (Option.to_list t.inst_attrib_value)
        ~f:(fun pos v -> Sbe.set_u32 b ~pos v)
    in
    ignore
      (write_group
         b
         ~pos
         ~block_length:Lot_type_rule.block_length
         t.lot_type_rules
         ~f:(fun pos (r : Lot_type_rule.t) ->
           Sbe.set_i8 b ~pos r.lot_type;
           Sbe.set_i32 b ~pos:(pos + 1) r.min_lot_size)
       : int)
  ;;

  let read b ~base ~block_length =
    let at o = base + o in
    let events, pos =
      read_group b ~pos:(at block_length) ~f:(fun pos : Event.t ->
        { event_type = Sbe.get_u8 b ~pos; event_time = Sbe.get_u64 b ~pos:(pos + 1) })
    in
    let feed_types, pos =
      read_group b ~pos ~f:(fun pos : Feed_type.t ->
        { md_feed_type = Sbe.get_string b ~pos ~len:3
        ; market_depth = Sbe.get_i8 b ~pos:(pos + 3)
        })
    in
    let inst_attribs, pos = read_group b ~pos ~f:(fun pos -> Sbe.get_u32 b ~pos) in
    let lot_type_rules, _ =
      read_group b ~pos ~f:(fun pos : Lot_type_rule.t ->
        { lot_type = Sbe.get_i8 b ~pos; min_lot_size = Sbe.get_i32 b ~pos:(pos + 1) })
    in
    { match_event_indicator = Sbe.get_u8 b ~pos:base
    ; tot_num_reports = Sbe.get_u32_opt b ~pos:(at 1)
    ; security_update_action = Sbe.get_char b ~pos:(at 5)
    ; last_update_time = Sbe.get_u64 b ~pos:(at 6)
    ; trading_status = get_status b ~pos:(at 14)
    ; appl_id = Sbe.get_i16 b ~pos:(at 15)
    ; market_segment_id = Sbe.get_u8 b ~pos:(at 17)
    ; underlying_product = Sbe.get_u8 b ~pos:(at 18)
    ; security_exchange = Sbe.get_string b ~pos:(at 19) ~len:4
    ; security_group = Sbe.get_string b ~pos:(at 23) ~len:6
    ; asset = Sbe.get_string b ~pos:(at 29) ~len:6
    ; symbol = Sbe.get_string b ~pos:(at 35) ~len:20
    ; security_id = Sbe.get_i32 b ~pos:(at 55)
    ; security_type = Sbe.get_string b ~pos:(at 59) ~len:6
    ; cfi_code = Sbe.get_string b ~pos:(at 65) ~len:6
    ; maturity_year = Sbe.get_u16 b ~pos:(at 71)
    ; maturity_month = Sbe.get_u8 b ~pos:(at 73)
    ; currency = Sbe.get_string b ~pos:(at 76) ~len:3
    ; settl_currency = Sbe.get_string b ~pos:(at 79) ~len:3
    ; match_algorithm = Sbe.get_char b ~pos:(at 82)
    ; min_trade_vol = Sbe.get_u32 b ~pos:(at 83)
    ; max_trade_vol = Sbe.get_u32 b ~pos:(at 87)
    ; min_price_increment = Sbe.get_price b ~pos:(at 91)
    ; display_factor = Sbe.get_price b ~pos:(at 99)
    ; unit_of_measure = Sbe.get_string b ~pos:(at 110) ~len:30
    ; unit_of_measure_qty = Sbe.get_price_opt b ~pos:(at 140)
    ; trading_reference_price = Sbe.get_price_opt b ~pos:(at 148)
    ; settl_price_type = Sbe.get_u8 b ~pos:(at 156)
    ; open_interest_qty = Sbe.get_i32_opt b ~pos:(at 157)
    ; cleared_volume = Sbe.get_i32_opt b ~pos:(at 161)
    ; high_limit_price = Sbe.get_price_opt b ~pos:(at 165)
    ; low_limit_price = Sbe.get_price_opt b ~pos:(at 173)
    ; max_price_variation = Sbe.get_price_opt b ~pos:(at 181)
    ; contract_multiplier = Sbe.get_i32_opt b ~pos:(at 199)
    ; min_price_increment_amount = Sbe.get_price_opt b ~pos:(at 205)
    ; trading_reference_date = Sbe.get_u16_opt b ~pos:(at 214)
    ; events
    ; feed_types
    ; inst_attrib_value = List.hd inst_attribs
    ; lot_type_rules
    }
  ;;
end

module Entry_type = struct
  type t =
    | Bid
    | Offer
    | Implied_bid
    | Implied_offer
    | Book_reset
  [@@deriving sexp, compare, equal, enumerate]

  let to_char = function
    | Bid -> '0'
    | Offer -> '1'
    | Implied_bid -> 'E'
    | Implied_offer -> 'F'
    | Book_reset -> 'J'
  ;;

  let of_char c = List.find all ~f:(fun t -> Char.equal (to_char t) c)
end

module Aggressor_side = struct
  type t =
    | No_aggressor
    | Buy
    | Sell
  [@@deriving sexp, compare, equal, enumerate]

  let to_int = function
    | No_aggressor -> 0
    | Buy -> 1
    | Sell -> 2
  ;;

  let of_int = function
    | 0 -> No_aggressor
    | 1 -> Buy
    | 2 -> Sell
    | v -> fail "invalid AggressorSide %d" v
  ;;
end

let get_update_action b ~pos =
  let v = Sbe.get_u8 b ~pos in
  match Update_action.of_int v with
  | Some a -> a
  | None -> fail "invalid MDUpdateAction %d" v
;;

let set_update_action b ~pos a = Sbe.set_u8 b ~pos (Update_action.to_int a)

module Incremental_book = struct
  module Entry = struct
    type t =
      { px : Price.t option
      ; size : int option
      ; security_id : int
      ; rpt_seq : int
      ; number_of_orders : int option
      ; price_level : int
      ; update_action : Update_action.t
      ; entry_type : Entry_type.t
      }
    [@@deriving sexp, compare, equal]

    let block_length = 32

    let write b ~pos t =
      Sbe.set_price_opt b ~pos t.px;
      Sbe.set_i32_opt b ~pos:(pos + 8) t.size;
      Sbe.set_i32 b ~pos:(pos + 12) t.security_id;
      Sbe.set_u32 b ~pos:(pos + 16) t.rpt_seq;
      Sbe.set_i32_opt b ~pos:(pos + 20) t.number_of_orders;
      Sbe.set_u8 b ~pos:(pos + 24) t.price_level;
      set_update_action b ~pos:(pos + 25) t.update_action;
      Sbe.set_char b ~pos:(pos + 26) (Entry_type.to_char t.entry_type);
      (* TradeableSize: implied quantities are out of scope *)
      Sbe.set_i32_opt b ~pos:(pos + 27) None
    ;;

    let read b ~pos =
      { px = Sbe.get_price_opt b ~pos
      ; size = Sbe.get_i32_opt b ~pos:(pos + 8)
      ; security_id = Sbe.get_i32 b ~pos:(pos + 12)
      ; rpt_seq = Sbe.get_u32 b ~pos:(pos + 16)
      ; number_of_orders = Sbe.get_i32_opt b ~pos:(pos + 20)
      ; price_level = Sbe.get_u8 b ~pos:(pos + 24)
      ; update_action = get_update_action b ~pos:(pos + 25)
      ; entry_type =
          (let c = Sbe.get_char b ~pos:(pos + 26) in
           match Entry_type.of_char c with
           | Some t -> t
           | None -> fail "invalid book MDEntryType %C" c)
      }
    ;;
  end

  type t =
    { transact_time : int
    ; match_event_indicator : Match_event.t
    ; entries : Entry.t list
    }
  [@@deriving sexp, compare, equal]

  let template_id = 46
  let block_length = 11
  let order_entry_length = 24

  (* NoOrderIDEntries is always empty: market by order is out of scope. *)
  let groups_size t =
    group_size ~block_length:Entry.block_length t.entries
    + group_size ~eight_byte:true ~block_length:order_entry_length []
  ;;

  let write b ~base t =
    Sbe.set_u64 b ~pos:base t.transact_time;
    Sbe.set_u8 b ~pos:(base + 8) t.match_event_indicator;
    let pos =
      write_group
        b
        ~pos:(base + block_length)
        ~block_length:Entry.block_length
        t.entries
        ~f:(fun pos e -> Entry.write b ~pos e)
    in
    ignore
      (write_group
         ~eight_byte:true
         b
         ~pos
         ~block_length:order_entry_length
         []
         ~f:(fun _ () -> ())
       : int)
  ;;

  let read b ~base ~block_length =
    let entries, _ =
      read_group b ~pos:(base + block_length) ~f:(fun pos -> Entry.read b ~pos)
    in
    { transact_time = Sbe.get_u64 b ~pos:base
    ; match_event_indicator = Sbe.get_u8 b ~pos:(base + 8)
    ; entries
    }
  ;;
end

module Trade_summary = struct
  module Entry = struct
    type t =
      { px : Price.t
      ; size : int
      ; security_id : int
      ; rpt_seq : int
      ; number_of_orders : int
      ; aggressor_side : Aggressor_side.t option
      ; update_action : Update_action.t
      ; md_trade_entry_id : int option
      }
    [@@deriving sexp, compare, equal]

    let block_length = 32

    let write b ~pos t =
      Sbe.set_price b ~pos t.px;
      Sbe.set_i32 b ~pos:(pos + 8) t.size;
      Sbe.set_i32 b ~pos:(pos + 12) t.security_id;
      Sbe.set_u32 b ~pos:(pos + 16) t.rpt_seq;
      Sbe.set_i32 b ~pos:(pos + 20) t.number_of_orders;
      Sbe.set_u8_opt
        b
        ~pos:(pos + 24)
        (Option.map t.aggressor_side ~f:Aggressor_side.to_int);
      set_update_action b ~pos:(pos + 25) t.update_action;
      Sbe.set_u32_opt b ~pos:(pos + 26) t.md_trade_entry_id
    ;;

    let read b ~pos =
      { px = Sbe.get_price b ~pos
      ; size = Sbe.get_i32 b ~pos:(pos + 8)
      ; security_id = Sbe.get_i32 b ~pos:(pos + 12)
      ; rpt_seq = Sbe.get_u32 b ~pos:(pos + 16)
      ; number_of_orders = Sbe.get_i32 b ~pos:(pos + 20)
      ; aggressor_side =
          Option.map (Sbe.get_u8_opt b ~pos:(pos + 24)) ~f:Aggressor_side.of_int
      ; update_action = get_update_action b ~pos:(pos + 25)
      ; md_trade_entry_id = Sbe.get_u32_opt b ~pos:(pos + 26)
      }
    ;;
  end

  module Order = struct
    type t =
      { order_id : int
      ; last_qty : int
      }
    [@@deriving sexp, compare, equal]

    let block_length = 16
  end

  type t =
    { transact_time : int
    ; match_event_indicator : Match_event.t
    ; entries : Entry.t list
    ; orders : Order.t list
    }
  [@@deriving sexp, compare, equal]

  let template_id = 48
  let block_length = 11

  let groups_size t =
    group_size ~block_length:Entry.block_length t.entries
    + group_size ~eight_byte:true ~block_length:Order.block_length t.orders
  ;;

  let write b ~base t =
    Sbe.set_u64 b ~pos:base t.transact_time;
    Sbe.set_u8 b ~pos:(base + 8) t.match_event_indicator;
    let pos =
      write_group
        b
        ~pos:(base + block_length)
        ~block_length:Entry.block_length
        t.entries
        ~f:(fun pos e -> Entry.write b ~pos e)
    in
    ignore
      (write_group
         ~eight_byte:true
         b
         ~pos
         ~block_length:Order.block_length
         t.orders
         ~f:(fun pos (o : Order.t) ->
           Sbe.set_u64 b ~pos o.order_id;
           Sbe.set_i32 b ~pos:(pos + 8) o.last_qty)
       : int)
  ;;

  let read b ~base ~block_length =
    let entries, pos =
      read_group b ~pos:(base + block_length) ~f:(fun pos -> Entry.read b ~pos)
    in
    let orders, _ =
      read_group ~eight_byte:true b ~pos ~f:(fun pos : Order.t ->
        { order_id = Sbe.get_u64 b ~pos; last_qty = Sbe.get_i32 b ~pos:(pos + 8) })
    in
    { transact_time = Sbe.get_u64 b ~pos:base
    ; match_event_indicator = Sbe.get_u8 b ~pos:(base + 8)
    ; entries
    ; orders
    }
  ;;
end

module Daily_statistics = struct
  module Entry = struct
    type t =
      { px : Price.t option
      ; size : int option
      ; security_id : int
      ; rpt_seq : int
      ; trading_reference_date : int option
      ; settl_price_type : int
      ; update_action : Update_action.t
      ; entry_type : char
      }
    [@@deriving sexp, compare, equal]

    let block_length = 32

    let write b ~pos t =
      Sbe.set_price_opt b ~pos t.px;
      Sbe.set_i32_opt b ~pos:(pos + 8) t.size;
      Sbe.set_i32 b ~pos:(pos + 12) t.security_id;
      Sbe.set_u32 b ~pos:(pos + 16) t.rpt_seq;
      Sbe.set_u16_opt b ~pos:(pos + 20) t.trading_reference_date;
      Sbe.set_u8 b ~pos:(pos + 22) t.settl_price_type;
      set_update_action b ~pos:(pos + 23) t.update_action;
      Sbe.set_char b ~pos:(pos + 24) t.entry_type
    ;;

    let read b ~pos =
      { px = Sbe.get_price_opt b ~pos
      ; size = Sbe.get_i32_opt b ~pos:(pos + 8)
      ; security_id = Sbe.get_i32 b ~pos:(pos + 12)
      ; rpt_seq = Sbe.get_u32 b ~pos:(pos + 16)
      ; trading_reference_date = Sbe.get_u16_opt b ~pos:(pos + 20)
      ; settl_price_type = Sbe.get_u8 b ~pos:(pos + 22)
      ; update_action = get_update_action b ~pos:(pos + 23)
      ; entry_type = Sbe.get_char b ~pos:(pos + 24)
      }
    ;;
  end

  type t =
    { transact_time : int
    ; match_event_indicator : Match_event.t
    ; entries : Entry.t list
    }
  [@@deriving sexp, compare, equal]

  let template_id = 49
  let block_length = 11
  let groups_size t = group_size ~block_length:Entry.block_length t.entries

  let write b ~base t =
    Sbe.set_u64 b ~pos:base t.transact_time;
    Sbe.set_u8 b ~pos:(base + 8) t.match_event_indicator;
    ignore
      (write_group
         b
         ~pos:(base + block_length)
         ~block_length:Entry.block_length
         t.entries
         ~f:(fun pos e -> Entry.write b ~pos e)
       : int)
  ;;

  let read b ~base ~block_length =
    let entries, _ =
      read_group b ~pos:(base + block_length) ~f:(fun pos -> Entry.read b ~pos)
    in
    { transact_time = Sbe.get_u64 b ~pos:base
    ; match_event_indicator = Sbe.get_u8 b ~pos:(base + 8)
    ; entries
    }
  ;;
end

module Snapshot_full_refresh = struct
  module Entry = struct
    type t =
      { px : Price.t option
      ; size : int option
      ; number_of_orders : int option
      ; price_level : int option
      ; trading_reference_date : int option
      ; open_close_settl_flag : int option
      ; settl_price_type : int
      ; entry_type : char
      }
    [@@deriving sexp, compare, equal]

    let block_length = 22

    let write b ~pos t =
      Sbe.set_price_opt b ~pos t.px;
      Sbe.set_i32_opt b ~pos:(pos + 8) t.size;
      Sbe.set_i32_opt b ~pos:(pos + 12) t.number_of_orders;
      Sbe.set_i8 b ~pos:(pos + 16) (Option.value t.price_level ~default:127);
      Sbe.set_u16_opt b ~pos:(pos + 17) t.trading_reference_date;
      Sbe.set_u8_opt b ~pos:(pos + 19) t.open_close_settl_flag;
      Sbe.set_u8 b ~pos:(pos + 20) t.settl_price_type;
      Sbe.set_char b ~pos:(pos + 21) t.entry_type
    ;;

    let read b ~pos =
      { px = Sbe.get_price_opt b ~pos
      ; size = Sbe.get_i32_opt b ~pos:(pos + 8)
      ; number_of_orders = Sbe.get_i32_opt b ~pos:(pos + 12)
      ; price_level =
          (match Sbe.get_i8 b ~pos:(pos + 16) with
           | 127 -> None
           | l -> Some l)
      ; trading_reference_date = Sbe.get_u16_opt b ~pos:(pos + 17)
      ; open_close_settl_flag = Sbe.get_u8_opt b ~pos:(pos + 19)
      ; settl_price_type = Sbe.get_u8 b ~pos:(pos + 20)
      ; entry_type = Sbe.get_char b ~pos:(pos + 21)
      }
    ;;
  end

  type t =
    { last_msg_seq_num_processed : int
    ; tot_num_reports : int
    ; security_id : int
    ; rpt_seq : int
    ; transact_time : int
    ; last_update_time : int
    ; trade_date : int option
    ; trading_status : Trading_status.t option
    ; high_limit_price : Price.t option
    ; low_limit_price : Price.t option
    ; max_price_variation : Price.t option
    ; entries : Entry.t list
    }
  [@@deriving sexp, compare, equal]

  let template_id = 52
  let block_length = 59
  let groups_size t = group_size ~block_length:Entry.block_length t.entries

  let write b ~base t =
    Sbe.set_u32 b ~pos:base t.last_msg_seq_num_processed;
    Sbe.set_u32 b ~pos:(base + 4) t.tot_num_reports;
    Sbe.set_i32 b ~pos:(base + 8) t.security_id;
    Sbe.set_u32 b ~pos:(base + 12) t.rpt_seq;
    Sbe.set_u64 b ~pos:(base + 16) t.transact_time;
    Sbe.set_u64 b ~pos:(base + 24) t.last_update_time;
    Sbe.set_u16_opt b ~pos:(base + 32) t.trade_date;
    set_status b ~pos:(base + 34) t.trading_status;
    Sbe.set_price_opt b ~pos:(base + 35) t.high_limit_price;
    Sbe.set_price_opt b ~pos:(base + 43) t.low_limit_price;
    Sbe.set_price_opt b ~pos:(base + 51) t.max_price_variation;
    ignore
      (write_group
         b
         ~pos:(base + block_length)
         ~block_length:Entry.block_length
         t.entries
         ~f:(fun pos e -> Entry.write b ~pos e)
       : int)
  ;;

  let read b ~base ~block_length =
    let entries, _ =
      read_group b ~pos:(base + block_length) ~f:(fun pos -> Entry.read b ~pos)
    in
    { last_msg_seq_num_processed = Sbe.get_u32 b ~pos:base
    ; tot_num_reports = Sbe.get_u32 b ~pos:(base + 4)
    ; security_id = Sbe.get_i32 b ~pos:(base + 8)
    ; rpt_seq = Sbe.get_u32 b ~pos:(base + 12)
    ; transact_time = Sbe.get_u64 b ~pos:(base + 16)
    ; last_update_time = Sbe.get_u64 b ~pos:(base + 24)
    ; trade_date = Sbe.get_u16_opt b ~pos:(base + 32)
    ; trading_status = get_status b ~pos:(base + 34)
    ; high_limit_price = Sbe.get_price_opt b ~pos:(base + 35)
    ; low_limit_price = Sbe.get_price_opt b ~pos:(base + 43)
    ; max_price_variation = Sbe.get_price_opt b ~pos:(base + 51)
    ; entries
    }
  ;;
end

module Message = struct
  type t =
    | Channel_reset of Channel_reset.t
    | Admin_heartbeat
    | Security_status of Security_status.t
    | Instrument_definition_future of Instrument_definition_future.t
    | Incremental_book of Incremental_book.t
    | Trade_summary of Trade_summary.t
    | Daily_statistics of Daily_statistics.t
    | Snapshot_full_refresh of Snapshot_full_refresh.t
  [@@deriving sexp, compare, equal]

  let template_id = function
    | Channel_reset _ -> Channel_reset.template_id
    | Admin_heartbeat -> 12
    | Security_status _ -> Security_status.template_id
    | Instrument_definition_future _ -> Instrument_definition_future.template_id
    | Incremental_book _ -> Incremental_book.template_id
    | Trade_summary _ -> Trade_summary.template_id
    | Daily_statistics _ -> Daily_statistics.template_id
    | Snapshot_full_refresh _ -> Snapshot_full_refresh.template_id
  ;;

  let name = function
    | Channel_reset _ -> "ChannelReset"
    | Admin_heartbeat -> "AdminHeartbeat"
    | Security_status _ -> "SecurityStatus"
    | Instrument_definition_future _ -> "MDInstrumentDefinitionFuture"
    | Incremental_book _ -> "MDIncrementalRefreshBook"
    | Trade_summary _ -> "MDIncrementalRefreshTradeSummary"
    | Daily_statistics _ -> "MDIncrementalRefreshDailyStatistics"
    | Snapshot_full_refresh _ -> "SnapshotFullRefresh"
  ;;

  (* Root block length and the size of what follows it. *)
  let sizes = function
    | Channel_reset t -> Channel_reset.block_length, Channel_reset.groups_size t
    | Admin_heartbeat -> 0, 0
    | Security_status _ -> Security_status.block_length, 0
    | Instrument_definition_future t ->
      ( Instrument_definition_future.block_length
      , Instrument_definition_future.groups_size t )
    | Incremental_book t -> Incremental_book.block_length, Incremental_book.groups_size t
    | Trade_summary t -> Trade_summary.block_length, Trade_summary.groups_size t
    | Daily_statistics t -> Daily_statistics.block_length, Daily_statistics.groups_size t
    | Snapshot_full_refresh t ->
      Snapshot_full_refresh.block_length, Snapshot_full_refresh.groups_size t
  ;;

  let write_body t b ~base =
    match t with
    | Channel_reset t -> Channel_reset.write b ~base t
    | Admin_heartbeat -> ()
    | Security_status t -> Security_status.write b ~base t
    | Instrument_definition_future t -> Instrument_definition_future.write b ~base t
    | Incremental_book t -> Incremental_book.write b ~base t
    | Trade_summary t -> Trade_summary.write b ~base t
    | Daily_statistics t -> Daily_statistics.write b ~base t
    | Snapshot_full_refresh t -> Snapshot_full_refresh.write b ~base t
  ;;
end

module Packet = struct
  type t =
    { msg_seq_num : int
    ; sending_time : int
    ; messages : Message.t list
    }
  [@@deriving sexp, compare, equal]

  let header_size = 12
end

(* MsgSize u16 and the SBE header. *)
let message_prefix = 2 + Sbe.Header.size

let message_size m =
  let block_length, extra = Message.sizes m in
  message_prefix + block_length + extra
;;

let write_message b ~pos m =
  let block_length, _ = Message.sizes m in
  let size = message_size m in
  Sbe.set_u16 b ~pos size;
  Sbe.Header.write
    b
    ~pos:(pos + 2)
    { block_length
    ; template_id = Message.template_id m
    ; schema_id
    ; version = schema_version
    };
  Message.write_body m b ~base:(pos + message_prefix);
  pos + size
;;

let encode_message m =
  let b = Bigstring.init (message_size m) ~f:(fun _ -> '\000') in
  ignore (write_message b ~pos:0 m : int);
  b
;;

let decode_message_exn b ~pos =
  let len = Bigstring.length b in
  if pos + message_prefix > len then fail "truncated message header at %d" pos;
  let size = Sbe.get_u16 b ~pos in
  if size < message_prefix || pos + size > len
  then fail "invalid MsgSize %d at %d (buffer %d)" size pos len;
  let header = Sbe.Header.read b ~pos:(pos + 2) in
  let base = pos + message_prefix in
  let need block_length =
    if header.block_length < block_length || message_prefix + header.block_length > size
    then
      fail
        "template %d: block length %d, expected at least %d"
        header.template_id
        header.block_length
        block_length
  in
  let m : Message.t option =
    if header.schema_id <> schema_id
    then fail "unexpected schema id %d" header.schema_id
    else (
      match header.template_id with
      | 4 ->
        need Channel_reset.block_length;
        Some
          (Channel_reset (Channel_reset.read b ~base ~block_length:header.block_length))
      | 12 -> Some Admin_heartbeat
      | 30 ->
        need Security_status.block_length;
        Some (Security_status (Security_status.read b ~base))
      | 54 ->
        need Instrument_definition_future.block_length;
        Some
          (Instrument_definition_future
             (Instrument_definition_future.read b ~base ~block_length:header.block_length))
      | 46 ->
        need Incremental_book.block_length;
        Some
          (Incremental_book
             (Incremental_book.read b ~base ~block_length:header.block_length))
      | 48 ->
        need Trade_summary.block_length;
        Some
          (Trade_summary (Trade_summary.read b ~base ~block_length:header.block_length))
      | 49 ->
        need Daily_statistics.block_length;
        Some
          (Daily_statistics
             (Daily_statistics.read b ~base ~block_length:header.block_length))
      | 52 ->
        need Snapshot_full_refresh.block_length;
        Some
          (Snapshot_full_refresh
             (Snapshot_full_refresh.read b ~base ~block_length:header.block_length))
      | _ -> None)
  in
  m, pos + size
;;

let wrap f =
  match f () with
  | v -> Ok v
  | exception Decode_error s -> Or_error.error_string s
  | exception exn -> Or_error.of_exn exn
;;

let decode_message b ~pos = wrap (fun () -> decode_message_exn b ~pos)

let write_packet b ~pos (p : Packet.t) =
  Sbe.set_u32 b ~pos p.msg_seq_num;
  Sbe.set_u64 b ~pos:(pos + 4) p.sending_time;
  List.fold p.messages ~init:(pos + Packet.header_size) ~f:(fun pos m ->
    write_message b ~pos m)
;;

let packet_size (p : Packet.t) =
  Packet.header_size + List.sum (module Int) p.messages ~f:message_size
;;

let encode_packet p =
  let b = Bigstring.init (packet_size p) ~f:(fun _ -> '\000') in
  ignore (write_packet b ~pos:0 p : int);
  b
;;

let decode_packet_at b ~pos ~stop : Packet.t =
  if pos + Packet.header_size > stop then fail "truncated packet header";
  let msg_seq_num = Sbe.get_u32 b ~pos in
  let sending_time = Sbe.get_u64 b ~pos:(pos + 4) in
  let rec loop pos acc =
    if pos >= stop
    then List.rev acc
    else (
      let m, next = decode_message_exn b ~pos in
      loop next (Option.fold m ~init:acc ~f:(fun acc m -> m :: acc)))
  in
  { msg_seq_num; sending_time; messages = loop (pos + Packet.header_size) [] }
;;

let decode_packet b =
  wrap (fun () -> decode_packet_at b ~pos:0 ~stop:(Bigstring.length b))
;;

let encode_frame p =
  let size = Sbe.Sofh.size + packet_size p in
  let b = Bigstring.init size ~f:(fun _ -> '\000') in
  Sbe.Sofh.write b ~pos:0 ~message_size:size;
  ignore (write_packet b ~pos:Sbe.Sofh.size p : int);
  b
;;

let decode_frame b =
  wrap (fun () ->
    let size = Or_error.ok_exn (Sbe.Sofh.read b ~pos:0) in
    if size > Bigstring.length b then fail "SOFH size %d exceeds the buffer" size;
    decode_packet_at b ~pos:Sbe.Sofh.size ~stop:size)
;;
