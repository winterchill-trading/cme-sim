open! Core
module Price = Exchange_core.Price
module Mdp3 = Cme_sim_wire.Mdp3

let px p = `String (Price.to_string p)
let opt f = Option.value_map ~default:`Null ~f
let int i = `Int i

let symbol security_id =
  match Products.find_security_id security_id with
  | Some p -> `String (Products.symbol p)
  | None -> `Null
;;

let status s =
  opt
    (fun s ->
      `String
        (match (s : Mdp3.Trading_status.t) with
         | Trading_halt -> "halt"
         | Close -> "close"
         | New_price_indication -> "new_price_indication"
         | Ready_to_trade -> "open"
         | Not_available_for_trading -> "not_available"
         | Unknown_or_invalid -> "unknown"
         | Pre_open -> "pre_open"
         | Pre_cross -> "pre_cross"
         | Cross -> "cross"
         | Post_close -> "post_close"
         | No_change -> "no_change"))
    s
;;

let action (a : Mdp3.Update_action.t) =
  `String
    (match a with
     | New -> "new"
     | Change -> "change"
     | Delete -> "delete"
     | Delete_thru -> "delete_thru"
     | Delete_from -> "delete_from"
     | Overlay -> "overlay")
;;

let entry_type (e : Mdp3.Entry_type.t) =
  `String
    (match e with
     | Bid -> "bid"
     | Offer -> "offer"
     | Implied_bid -> "implied_bid"
     | Implied_offer -> "implied_offer"
     | Book_reset -> "book_reset")
;;

let snapshot_entry_type c =
  `String
    (match c with
     | '0' -> "bid"
     | '1' -> "offer"
     | '2' -> "trade"
     | '6' -> "settlement"
     | c -> String.of_char c)
;;

let fields (m : Mdp3.Message.t) : (string * Yojson.Safe.t) list =
  match m with
  | Channel_reset c ->
    [ "type", `String "channel_reset"; "transact_time", int c.transact_time ]
  | Admin_heartbeat -> [ "type", `String "heartbeat" ]
  | Security_status s ->
    [ "type", `String "status"
    ; "symbol", opt symbol s.security_id
    ; "security_id", opt int s.security_id
    ; "security_group", `String s.security_group
    ; "status", status s.trading_status
    ; "transact_time", int s.transact_time
    ]
  | Instrument_definition_future d ->
    [ "type", `String "definition"
    ; "symbol", `String d.symbol
    ; "security_id", int d.security_id
    ; "security_group", `String d.security_group
    ; "exchange", `String d.security_exchange
    ; "maturity", `String (sprintf "%04d%02d" d.maturity_year d.maturity_month)
    ; "tick", px d.min_price_increment
    ; "multiplier", opt int d.contract_multiplier
    ; "tick_value", opt px d.min_price_increment_amount
    ; "unit_of_measure", `String d.unit_of_measure
    ; "reference_price", opt px d.trading_reference_price
    ; "low_limit", opt px d.low_limit_price
    ; "high_limit", opt px d.high_limit_price
    ; "status", status d.trading_status
    ]
  | Incremental_book b ->
    [ "type", `String "book"
    ; "transact_time", int b.transact_time
    ; ( "entries"
      , `List
          (List.map b.entries ~f:(fun e ->
             `Assoc
               [ "symbol", symbol e.security_id
               ; "side", entry_type e.entry_type
               ; "level", int e.price_level
               ; "action", action e.update_action
               ; "price", opt px e.px
               ; "size", opt int e.size
               ; "orders", opt int e.number_of_orders
               ; "rpt_seq", int e.rpt_seq
               ])) )
    ]
  | Trade_summary t ->
    [ "type", `String "trade"
    ; "transact_time", int t.transact_time
    ; ( "entries"
      , `List
          (List.map t.entries ~f:(fun e ->
             `Assoc
               [ "symbol", symbol e.security_id
               ; "price", px e.px
               ; "size", int e.size
               ; ( "aggressor"
                 , opt
                     (fun (a : Mdp3.Aggressor_side.t) ->
                       match a with
                       | No_aggressor -> `Null
                       | Buy -> `String "buy"
                       | Sell -> `String "sell")
                     e.aggressor_side )
               ; "orders", int e.number_of_orders
               ; "trade_id", opt int e.md_trade_entry_id
               ; "rpt_seq", int e.rpt_seq
               ])) )
    ]
  | Daily_statistics d ->
    [ "type", `String "daily_statistics"
    ; ( "entries"
      , `List
          (List.map d.entries ~f:(fun e ->
             `Assoc
               [ "symbol", symbol e.security_id
               ; "stat", snapshot_entry_type e.entry_type
               ; "price", opt px e.px
               ; "size", opt int e.size
               ; "rpt_seq", int e.rpt_seq
               ])) )
    ]
  | Snapshot_full_refresh s ->
    [ "type", `String "snapshot"
    ; "symbol", symbol s.security_id
    ; "security_id", int s.security_id
    ; "rpt_seq", int s.rpt_seq
    ; "last_seq", int s.last_msg_seq_num_processed
    ; "status", status s.trading_status
    ; "low_limit", opt px s.low_limit_price
    ; "high_limit", opt px s.high_limit_price
    ; ( "entries"
      , `List
          (List.map s.entries ~f:(fun e ->
             `Assoc
               [ "kind", snapshot_entry_type e.entry_type
               ; "level", opt int e.price_level
               ; "price", opt px e.px
               ; "size", opt int e.size
               ; "orders", opt int e.number_of_orders
               ])) )
    ]
;;

let of_packet (p : Mdp3.Packet.t) =
  List.map p.messages ~f:(fun m ->
    `Assoc ([ "seq", `Int p.msg_seq_num; "time", `Int p.sending_time ] @ fields m))
;;
