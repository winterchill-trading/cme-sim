open! Core
open Cme_sim_wire
open Mdp3
module Price = Exchange_core.Price

let px = Price.of_string

let es : Instrument_definition_future.t =
  { match_event_indicator = Match_event.end_of_event
  ; tot_num_reports = Some 4
  ; security_update_action = 'A'
  ; last_update_time = 1_760_000_000_000_000_000
  ; trading_status = Some Ready_to_trade
  ; appl_id = 310
  ; market_segment_id = 64
  ; underlying_product = 5
  ; security_exchange = "XCME"
  ; security_group = "ES"
  ; asset = "ES"
  ; symbol = "ESZ6"
  ; security_id = 1001
  ; security_type = "FUT"
  ; cfi_code = "FFIXSX"
  ; maturity_year = 2026
  ; maturity_month = 12
  ; currency = "USD"
  ; settl_currency = "USD"
  ; match_algorithm = 'F'
  ; min_trade_vol = 1
  ; max_trade_vol = 3000
  ; min_price_increment = px "0.25"
  ; display_factor = px "1"
  ; unit_of_measure = "IPNT"
  ; unit_of_measure_qty = Some (px "50")
  ; trading_reference_price = Some (px "6000")
  ; settl_price_type = 0b0011
  ; open_interest_qty = None
  ; cleared_volume = None
  ; high_limit_price = Some (px "6420")
  ; low_limit_price = Some (px "5580")
  ; max_price_variation = Some (px "120")
  ; contract_multiplier = Some 50
  ; min_price_increment_amount = Some (px "12.5")
  ; trading_reference_date = Some 20_373
  ; events = [ { event_type = 7; event_time = 1_797_000_000_000_000_000 } ]
  ; feed_types = [ { md_feed_type = "GBX"; market_depth = 10 } ]
  ; inst_attrib_value = Some 0x1
  ; lot_type_rules = [ { lot_type = 2; min_lot_size = 10_000 } ]
  }
;;

let%expect_test "MDInstrumentDefinitionFuture offsets from the schema" =
  let b = encode_message (Instrument_definition_future es) in
  let at o = 10 + o in
  print_s
    [%message
      ""
        ~msg_size:(Sbe.get_u16 b ~pos:0 : int)
        ~header:(Sbe.Header.read b ~pos:2 : Sbe.Header.t)
        ~symbol:(Sbe.get_string b ~pos:(at 35) ~len:20 : string)
        ~security_id:(Sbe.get_i32 b ~pos:(at 55) : int)
        ~security_type:(Sbe.get_string b ~pos:(at 59) ~len:6 : string)
        ~cfi:(Sbe.get_string b ~pos:(at 65) ~len:6 : string)
        ~maturity:((Sbe.get_u16 b ~pos:(at 71), Sbe.get_u8 b ~pos:(at 73)) : int * int)
        ~currency:(Sbe.get_string b ~pos:(at 76) ~len:3 : string)
        ~min_trade_vol:(Sbe.get_u32 b ~pos:(at 83) : int)
        ~max_trade_vol:(Sbe.get_u32 b ~pos:(at 87) : int)
        ~tick_mantissa:(Sbe.get_i64 b ~pos:(at 91) : Int64.t)
        ~unit_of_measure_qty:(Sbe.get_price b ~pos:(at 140) : Price.t)
        ~trading_reference_price:(Sbe.get_price b ~pos:(at 148) : Price.t)
        ~high_limit:(Sbe.get_price b ~pos:(at 165) : Price.t)
        ~low_limit:(Sbe.get_price b ~pos:(at 173) : Price.t)
        ~max_price_variation:(Sbe.get_price b ~pos:(at 181) : Price.t)
        ~contract_multiplier:(Sbe.get_i32 b ~pos:(at 199) : int)
        ~min_price_increment_amount:(Sbe.get_price b ~pos:(at 205) : Price.t)
        ~no_events:(Sbe.Group.read b ~pos:(at 224) : int * int * int)];
  [%expect
    {|
    ((msg_size 268)
     (header ((block_length 224) (template_id 54) (schema_id 1) (version 13)))
     (symbol ESZ6) (security_id 1001) (security_type FUT) (cfi FFIXSX)
     (maturity (2026 12)) (currency USD) (min_trade_vol 1) (max_trade_vol 3000)
     (tick_mantissa 250000000) (unit_of_measure_qty 50)
     (trading_reference_price 6000) (high_limit 6420) (low_limit 5580)
     (max_price_variation 120) (contract_multiplier 50)
     (min_price_increment_amount 12.5) (no_events (9 1 237)))
    |}]
;;

let status : Security_status.t =
  { transact_time = 1_760_000_000_000_000_000
  ; security_group = "ES"
  ; asset = "ES"
  ; security_id = Some 1001
  ; trade_date = Some 20_373
  ; match_event_indicator = Match_event.end_of_event
  ; trading_status = Some Pre_open
  ; halt_reason = 0
  ; trading_event = 0
  }
;;

let%expect_test "SecurityStatus layout" =
  let b = encode_message (Security_status status) in
  print_endline (Sbe.hex b);
  [%expect
    {|
    28 00 1e 00 1e 00 01 00 0d 00 00 00 b0 d4 ac c6
    6c 18 45 53 00 00 00 00 45 53 00 00 00 00 e9 03
    00 00 95 4f 80 15 00 00
    |}]
;;

let%expect_test "packets and TCP frames round trip" =
  let p : Packet.t =
    { msg_seq_num = 1
    ; sending_time = 1_760_000_000_000_000_000
    ; messages =
        [ Channel_reset
            { transact_time = 5
            ; match_event_indicator = Match_event.end_of_event
            ; appl_ids = [ 310 ]
            }
        ; Admin_heartbeat
        ; Security_status status
        ; Instrument_definition_future es
        ; Instrument_definition_future
            { es with
              events = []
            ; feed_types = []
            ; inst_attrib_value = None
            ; lot_type_rules = []
            ; high_limit_price = None
            ; trading_status = None
            }
        ]
    }
  in
  let frame = encode_frame p in
  print_s
    [%message
      ""
        ~frame_size:(Bigstring.length frame : int)
        ~packet_size:(Bigstring.length (encode_packet p) : int)
        ~sofh:(Sbe.Sofh.read frame ~pos:0 : int Or_error.t)
        ~seq:(Sbe.get_u32 frame ~pos:4 : int)
        ~frame_round_trip:
          (Or_error.map (decode_frame frame) ~f:(Packet.equal p) : bool Or_error.t)
        ~packet_round_trip:
          (Or_error.map (decode_packet (encode_packet p)) ~f:(Packet.equal p)
           : bool Or_error.t)];
  [%expect
    {|
    ((frame_size 604) (packet_size 600) (sofh (Ok 604)) (seq 1)
     (frame_round_trip (Ok true)) (packet_round_trip (Ok true)))
    |}]
;;

let%expect_test "unknown templates are skipped" =
  let b = encode_message Admin_heartbeat in
  Sbe.set_u16 b ~pos:4 99;
  print_s [%sexp (decode_message b ~pos:0 : (Message.t option * int) Or_error.t)];
  [%expect {| (Ok (() 10)) |}]
;;

let%expect_test "match event names" =
  print_s
    [%sexp
      (Match_event.to_names (Match_event.last_quote_msg lor Match_event.end_of_event)
       : string list)];
  [%expect {| (LastQuoteMsg EndOfEvent) |}]
;;
