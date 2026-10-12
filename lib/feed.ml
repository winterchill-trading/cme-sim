open! Core
module Price = Exchange_core.Price
module Size = Exchange_core.Size
module Mdp3 = Cme_sim_wire.Mdp3
module Mbp_book = Cme_sim_wire.Mbp_book

let channel = 310
let depth = 10

type instrument =
  { product : Products.t
  ; mutable rpt_seq : int
  ; mutable bids : Mbp_book.Level.t list
  ; mutable offers : Mbp_book.Level.t list
  ; mutable last_trade : (Price.t * int) option
  ; mutable settlement : Price.t option
  }

type t =
  { venue : Venue.t
  ; instruments : instrument Int.Table.t
  ; subscribers : (Mdp3.Packet.t -> unit) Int.Table.t
  ; mutable next_subscriber : int
  ; mutable msg_seq_num : int
  ; mutable last_published : int
  }

let create venue =
  let instruments = Int.Table.create () in
  List.iter Products.all ~f:(fun product ->
    Hashtbl.set
      instruments
      ~key:product.security_id
      ~data:
        { product
        ; rpt_seq = 0
        ; bids = []
        ; offers = []
        ; last_trade = None
        ; settlement = Some product.reference_price
        });
  { venue
  ; instruments
  ; subscribers = Int.Table.create ()
  ; next_subscriber = 0
  ; msg_seq_num = 0
  ; last_published = 0
  }
;;

let msg_seq_num t = t.msg_seq_num

let status_of_phase : Venue.Phase.t -> Mdp3.Trading_status.t = function
  | Open -> Ready_to_trade
  | Pre_open -> Pre_open
  | Halted -> Trading_halt
  | Closed -> Close
;;

let epoch = Date.of_string "1970-01-01"

(* Trade date as days since the epoch: the Globex session's date, else the UTC date. *)
let trade_date ~now =
  let time = Time_ns.of_int_ns_since_epoch now in
  let date =
    match Equities_core.Session.Cme_globex.trade_date time with
    | Some d -> d
    | None -> Time_ns.to_date time ~zone:Time_float.Zone.utc
  in
  Date.diff date epoch
;;

let broadcast t packet = Hashtbl.iter t.subscribers ~f:(fun f -> f packet)

let publish t ~now message =
  t.msg_seq_num <- t.msg_seq_num + 1;
  t.last_published <- now;
  broadcast
    t
    { Mdp3.Packet.msg_seq_num = t.msg_seq_num
    ; sending_time = now
    ; messages = [ message ]
    }
;;

let next_rpt_seq i =
  i.rpt_seq <- i.rpt_seq + 1;
  i.rpt_seq
;;

let contracts size = Size.to_units size / Size.units_per_one

let engine_levels (levels : Exchange_core.L2.Level.t list) =
  List.map levels ~f:(fun (l : Exchange_core.L2.Level.t) : Mbp_book.Level.t ->
    { px = l.px; size = contracts l.sz; orders = l.n })
;;

let on_book_changed t ~now i =
  let bids, offers =
    match
      Exchange_core_engine.Exchange.l2
        ~n_levels:depth
        (Venue.exchange t.venue)
        (Products.engine_symbol i.product)
    with
    | None -> [], []
    | Some l2 -> engine_levels l2.bids, engine_levels l2.asks
  in
  let updates =
    Mbp_book.diff Bid ~old:i.bids ~new_:bids
    @ Mbp_book.diff Offer ~old:i.offers ~new_:offers
  in
  i.bids <- bids;
  i.offers <- offers;
  if not (List.is_empty updates)
  then
    publish
      t
      ~now
      (Incremental_book
         { transact_time = now
         ; match_event_indicator = Mdp3.Match_event.(last_quote_msg lor end_of_event)
         ; entries =
             List.map updates ~f:(fun u ->
               Mbp_book.Update.to_entry
                 u
                 ~security_id:i.product.security_id
                 ~rpt_seq:(next_rpt_seq i))
         })
;;

let on_trade t ~now i (trade : Venue.Trade.t) =
  i.last_trade <- Some (trade.px, trade.size);
  let aggressor : Mdp3.Aggressor_side.t =
    match trade.aggressor with
    | None -> No_aggressor
    | Some Buy -> Buy
    | Some Sell -> Sell
  in
  publish
    t
    ~now
    (Trade_summary
       { transact_time = trade.time
       ; match_event_indicator = Mdp3.Match_event.(last_trade_msg lor end_of_event)
       ; entries =
           [ { px = trade.px
             ; size = trade.size
             ; security_id = trade.security_id
             ; rpt_seq = next_rpt_seq i
             ; number_of_orders =
                 List.length trade.orders + Bool.to_int (Option.is_some trade.aggressor)
             ; aggressor_side = Some aggressor
             ; update_action = New
             ; md_trade_entry_id = Some trade.md_trade_entry_id
             }
           ]
       ; orders =
           List.map trade.orders ~f:(fun (order_id, last_qty) ->
             { Mdp3.Trade_summary.Order.order_id; last_qty })
       })
;;

let security_status ~now i phase : Mdp3.Message.t =
  Security_status
    { transact_time = now
    ; security_group = Products.security_group i.product
    ; asset = Products.security_group i.product
    ; security_id = Some i.product.security_id
    ; trade_date = Some (trade_date ~now)
    ; match_event_indicator = Mdp3.Match_event.end_of_event
    ; trading_status = Some (status_of_phase phase)
    ; halt_reason = 0
    ; trading_event = 0
    }
;;

(* SettlPriceType bits: final (0) and actual (1). *)
let final_actual = 0b11

let on_output t ~now (o : Venue.Output.t) =
  let instrument id = Hashtbl.find t.instruments id in
  match o with
  | Report _ -> ()
  | Trade trade ->
    Option.iter (instrument trade.security_id) ~f:(fun i -> on_trade t ~now i trade)
  | Book_changed { security_id } ->
    Option.iter (instrument security_id) ~f:(on_book_changed t ~now)
  | Phase_changed { security_id; phase; time } ->
    Option.iter (instrument security_id) ~f:(fun i ->
      publish t ~now (security_status ~now:time i phase))
  | Settlement { security_id; px; time } ->
    Option.iter (instrument security_id) ~f:(fun i ->
      i.settlement <- Some px;
      publish
        t
        ~now
        (Daily_statistics
           { transact_time = time
           ; match_event_indicator = Mdp3.Match_event.(last_stats_msg lor end_of_event)
           ; entries =
               [ { px = Some px
                 ; size = None
                 ; security_id
                 ; rpt_seq = next_rpt_seq i
                 ; trading_reference_date = Some (trade_date ~now:time)
                 ; settl_price_type = final_actual
                 ; update_action = New
                 ; entry_type = '6'
                 }
               ]
           }))
;;

let heartbeat_if_idle t ~now ~interval =
  if now - t.last_published >= Time_ns.Span.to_int_ns interval
  then publish t ~now Admin_heartbeat
;;

let underlying_product (p : Products.t) =
  match p.product.root with
  | "ES" | "NQ" -> 5 (* equity *)
  | "CL" -> 16 (* energy *)
  | _ -> 17 (* metals *)
;;

let security_exchange (p : Products.t) =
  match p.product.root with
  | "ES" | "NQ" -> "XCME"
  | "CL" -> "XNYM"
  | _ -> "XCEC"
;;

let definition t ~now (p : Products.t) : Mdp3.Instrument_definition_future.t =
  let low, high = Venue.price_limits t.venue ~security_id:p.security_id in
  let multiplier = Products.multiplier p in
  let last_trade_ns =
    Equities_core.Session.Zone.of_local
      Chicago
      (Products.last_trade_date p)
      (Equities_core.Session.hm 13 30)
    |> Time_ns.to_int_ns_since_epoch
  in
  { match_event_indicator = Mdp3.Match_event.end_of_event
  ; tot_num_reports = Some (List.length Products.all)
  ; security_update_action = 'A'
  ; last_update_time = now
  ; trading_status =
      Some (status_of_phase (Venue.phase t.venue ~security_id:p.security_id))
  ; appl_id = channel
  ; market_segment_id = 64
  ; underlying_product = underlying_product p
  ; security_exchange = security_exchange p
  ; security_group = Products.security_group p
  ; asset = Products.security_group p
  ; symbol = Products.symbol p
  ; security_id = p.security_id
  ; security_type = "FUT"
  ; cfi_code = "FFICSX"
  ; maturity_year = p.future.year
  ; maturity_month = Month.to_int p.future.month
  ; currency = "USD"
  ; settl_currency = "USD"
  ; match_algorithm = 'F'
  ; min_trade_vol = 1
  ; max_trade_vol = p.max_trade_vol
  ; min_price_increment = Products.tick p
  ; display_factor = Price.one
  ; unit_of_measure = p.unit_of_measure
  ; unit_of_measure_qty = Some (Price.of_int multiplier)
  ; trading_reference_price = Some (Venue.last_price t.venue ~security_id:p.security_id)
  ; settl_price_type = final_actual
  ; open_interest_qty = None
  ; cleared_volume = None
  ; high_limit_price = Some high
  ; low_limit_price = Some low
  ; max_price_variation = Some p.protection
  ; contract_multiplier = Some multiplier
  ; min_price_increment_amount = Some (Price.scale (Products.tick p) ~by:multiplier)
  ; trading_reference_date = Some (trade_date ~now)
  ; events =
      [ { event_type = 5
        ; event_time =
            Time_ns.to_int_ns_since_epoch (Time_ns.of_string "2025-09-19 13:30:00Z")
        }
      ; { event_type = 7; event_time = last_trade_ns }
      ]
  ; feed_types = [ { md_feed_type = "GBX"; market_depth = depth } ]
  ; inst_attrib_value = Some 0b11 (* electronic match and order cross eligible *)
  ; lot_type_rules = [ { lot_type = 2; min_lot_size = 10_000 } ]
  }
;;

let snapshot t ~now i : Mdp3.Message.t =
  let low, high = Venue.price_limits t.venue ~security_id:i.product.security_id in
  let side c levels =
    List.mapi
      levels
      ~f:(fun n (l : Mbp_book.Level.t) : Mdp3.Snapshot_full_refresh.Entry.t ->
        { px = Some l.px
        ; size = Some l.size
        ; number_of_orders = Some l.orders
        ; price_level = Some (n + 1)
        ; trading_reference_date = None
        ; open_close_settl_flag = None
        ; settl_price_type = 0
        ; entry_type = c
        })
  in
  let last_trade =
    Option.to_list i.last_trade
    |> List.map ~f:(fun (px, size) : Mdp3.Snapshot_full_refresh.Entry.t ->
      { px = Some px
      ; size = Some size
      ; number_of_orders = None
      ; price_level = None
      ; trading_reference_date = None
      ; open_close_settl_flag = None
      ; settl_price_type = 0
      ; entry_type = '2'
      })
  in
  let settlement =
    Option.to_list i.settlement
    |> List.map ~f:(fun px : Mdp3.Snapshot_full_refresh.Entry.t ->
      { px = Some px
      ; size = None
      ; number_of_orders = None
      ; price_level = None
      ; trading_reference_date = Some (trade_date ~now)
      ; open_close_settl_flag = None
      ; settl_price_type = final_actual
      ; entry_type = '6'
      })
  in
  Snapshot_full_refresh
    { last_msg_seq_num_processed = t.msg_seq_num
    ; tot_num_reports = Hashtbl.length t.instruments
    ; security_id = i.product.security_id
    ; rpt_seq = i.rpt_seq
    ; transact_time = now
    ; last_update_time = now
    ; trade_date = Some (trade_date ~now)
    ; trading_status =
        Some (status_of_phase (Venue.phase t.venue ~security_id:i.product.security_id))
    ; high_limit_price = Some high
    ; low_limit_price = Some low
    ; max_price_variation = Some i.product.protection
    ; entries = side '0' i.bids @ side '1' i.offers @ last_trade @ settlement
    }
;;

let subscribe t ~now f =
  let packet m =
    { Mdp3.Packet.msg_seq_num = t.msg_seq_num; sending_time = now; messages = [ m ] }
  in
  let instruments =
    Hashtbl.data t.instruments
    |> List.sort ~compare:(fun a b ->
      Int.compare a.product.security_id b.product.security_id)
  in
  List.iter instruments ~f:(fun i ->
    f (packet (Instrument_definition_future (definition t ~now i.product))));
  List.iter instruments ~f:(fun i ->
    f
      (packet
         (security_status ~now i (Venue.phase t.venue ~security_id:i.product.security_id))));
  List.iter instruments ~f:(fun i -> f (packet (snapshot t ~now i)));
  let id = t.next_subscriber in
  t.next_subscriber <- id + 1;
  Hashtbl.set t.subscribers ~key:id ~data:f;
  fun () -> Hashtbl.remove t.subscribers id
;;
