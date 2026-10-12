open! Core
module Account = Exchange_core.Account
module Client_order_id = Exchange_core.Client_order_id
module Order_id = Exchange_core.Order_id
module Price = Exchange_core.Price
module Size = Exchange_core.Size
module Usd = Exchange_core.Usd
module Exchange = Exchange_core_engine.Exchange
module Event = Exchange_core_engine.Event
module Order_types = Equities_core.Order_types
module Ilink3 = Cme_sim_wire.Ilink3

module Phase = struct
  type t =
    | Open
    | Pre_open
    | Halted
    | Closed
  [@@deriving sexp, compare, equal, enumerate]
end

module Trade = struct
  type t =
    { security_id : int
    ; px : Price.t
    ; size : int
    ; aggressor : Ilink3.Side.t option
    ; md_trade_entry_id : int
    ; orders : (int * int) list
    ; time : int
    }
  [@@deriving sexp_of]
end

module Output = struct
  type t =
    | Report of
        { account : Account.t
        ; message : Ilink3.Message.t
        }
    | Trade of Trade.t
    | Book_changed of { security_id : int }
    | Phase_changed of
        { security_id : int
        ; phase : Phase.t
        ; time : int
        }
    | Settlement of
        { security_id : int
        ; px : Price.t
        ; time : int
        }
  [@@deriving sexp_of]
end

(* FIX OrdRejReason / CxlRejReason values used in rejects. *)
module Reason = struct
  let unknown_symbol = 1
  let exchange_closed = 2
  let exceeds_limit = 3
  let unsupported = 11
  let incorrect_quantity = 13
  let invalid_price_increment = 18
  let other = 99
  let too_late_to_cancel = 0
  let cxl_unknown_order = 1
end

(* ExecRestatementReason values. *)
let cancel_on_disconnect_reason = 100
let smp_newest_cancelled = 107

type order =
  { order_id : int
  ; account : Account.t
  ; product : Products.t
  ; side : Ilink3.Side.t
  ; ord_type : Ilink3.Ord_type.t
  ; tif : Ilink3.Time_in_force.t
  ; min_qty : int option
  ; manual : bool
  ; arrival : int
  ; mutable display_qty : int option
  ; mutable price : Price.t
  ; mutable order_qty : int
  ; mutable cum_qty : int
  ; mutable cl_ord_id : string
  ; mutable sender_id : string
  ; mutable location : string
  ; party : int
  ; mutable order_request_id : int
  ; mutable engine_oid : Order_id.t option
  ; mutable slice_left : int (** unfilled part of the slice resting in the engine *)
  ; mutable queued : bool
  }

(* Fills of one aggressing order at one price form one trade summary entry. *)
type trade_group =
  { g_security_id : int
  ; g_taker : Order_id.t
  ; g_px : Price.t
  ; g_aggressor : Ilink3.Side.t
  ; g_id : int
  ; mutable g_size : int
  ; mutable g_orders : (int * int) list (** reversed *)
  }

type t =
  { exchange : Exchange.t
  ; deposit : Usd.t
  ; funded : Account.Hash_set.t
  ; orders : (int, order) Hashtbl.t
  ; by_engine : (Order_id.t, int) Hashtbl.t
  ; phases : (int, Phase.t) Hashtbl.t
  ; references : (int, Price.t) Hashtbl.t
  ; last_trades : (int, Price.t) Hashtbl.t
  ; mutable outputs : Output.t list
  ; mutable now : int
  ; mutable next_order_id : int
  ; mutable next_exec_id : int
  ; mutable next_trade_entry_id : int
  ; mutable next_side_trade_id : int
  ; mutable next_arrival : int
  ; mutable group : trade_group option
  ; changed_books : Int.Hash_set.t
  ; refills : order Queue.t
  ; refill_slices : Order_id.Hash_set.t
  }

let product_of_symbol symbol =
  List.find_exn Products.all ~f:(fun p ->
    Exchange_core.Symbol.equal (Products.engine_symbol p) symbol)
;;

let create ?(deposit = Usd.of_int 1_000_000_000) () =
  let config : Exchange.Config.t =
    { Exchange.Config.default with
      instruments = List.map Products.all ~f:Products.instrument
    ; default_leverage = 50
    ; funding_interval_ns = Int.max_value / 2
    ; default_funding_rate = 0.
    ; min_order_value = Usd.zero
    ; l2_levels = 10
    }
  in
  let t =
    { exchange = Exchange.create ~config ()
    ; deposit
    ; funded = Account.Hash_set.create ()
    ; orders = Hashtbl.create (module Int)
    ; by_engine = Hashtbl.create (module Order_id)
    ; phases = Hashtbl.create (module Int)
    ; references = Hashtbl.create (module Int)
    ; last_trades = Hashtbl.create (module Int)
    ; outputs = []
    ; now = 0
    ; next_order_id = 700_000_000_001
    ; next_exec_id = 1
    ; next_trade_entry_id = 1
    ; next_side_trade_id = 1
    ; next_arrival = 0
    ; group = None
    ; changed_books = Int.Hash_set.create ()
    ; refills = Queue.create ()
    ; refill_slices = Order_id.Hash_set.create ()
    }
  in
  List.iter Products.all ~f:(fun p ->
    Hashtbl.set t.phases ~key:p.security_id ~data:Open;
    Hashtbl.set t.references ~key:p.security_id ~data:p.reference_price);
  t
;;

let exchange t = t.exchange

let phase t ~security_id =
  Hashtbl.find t.phases security_id |> Option.value ~default:Closed
;;

let reference t ~security_id =
  match Hashtbl.find t.references security_id with
  | Some p -> p
  | None -> (Option.value_exn (Products.find_security_id security_id)).reference_price
;;

let price_limits t ~security_id =
  let product = Option.value_exn (Products.find_security_id security_id) in
  Products.price_limits product ~reference:(reference t ~security_id)
;;

let last_price t ~security_id =
  match Hashtbl.find t.last_trades security_id with
  | Some p -> p
  | None -> reference t ~security_id
;;

let set_time t now =
  if now > t.now
  then (
    t.now <- now;
    Exchange.step t.exchange ~now)
;;

let emit t o = t.outputs <- o :: t.outputs

let drain t =
  let o = List.rev t.outputs in
  t.outputs <- [];
  o
;;

let report t account message = emit t (Report { account; message })

let fund t account =
  if not (Hash_set.mem t.funded account)
  then (
    Hash_set.add t.funded account;
    Exchange.deposit t.exchange account t.deposit)
;;

let next_exec_id t =
  let id = t.next_exec_id in
  t.next_exec_id <- id + 1;
  sprintf "X%d" id
;;

let leaves o = o.order_qty - o.cum_qty
let is_ioc o = Ilink3.Time_in_force.(equal o.tif Fak || equal o.tif Fok)

let order_side : Ilink3.Side.t -> Order_types.Side.t = function
  | Buy -> Buy
  | Sell -> Sell
;;

let ilink_side : Exchange_core.Side.t -> Ilink3.Side.t = function
  | Buy -> Buy
  | Sell -> Sell
;;

(* {2 Reports} *)

let common t o : Ilink3.Exec_common.t =
  { seq_num = 0
  ; uuid = 0
  ; exec_id = next_exec_id t
  ; sender_id = o.sender_id
  ; cl_ord_id = o.cl_ord_id
  ; party_details_list_req_id = o.party
  ; order_id = o.order_id
  ; transact_time = t.now
  ; sending_time = t.now
  ; order_request_id = o.order_request_id
  ; location = o.location
  ; security_id = o.product.security_id
  ; side = o.side
  ; manual = o.manual
  ; poss_retrans = false
  }
;;

let report_new t o =
  report
    t
    o.account
    (Execution_report_new
       { common = common t o
       ; price = o.price
       ; stop_px = None
       ; order_qty = o.order_qty
       ; min_qty = o.min_qty
       ; display_qty = o.display_qty
       ; ord_type = o.ord_type
       ; time_in_force = o.tif
       })
;;

let report_elimination t o =
  report
    t
    o.account
    (Execution_report_elimination
       { common = common t o
       ; price = o.price
       ; cum_qty = o.cum_qty
       ; order_qty = o.order_qty
       ; min_qty = o.min_qty
       ; display_qty = o.display_qty
       ; ord_type = o.ord_type
       ; time_in_force = o.tif
       })
;;

let report_cancel ?reason t o =
  report
    t
    o.account
    (Execution_report_cancel
       { common = common t o
       ; price = o.price
       ; stop_px = None
       ; order_qty = o.order_qty
       ; cum_qty = o.cum_qty
       ; min_qty = o.min_qty
       ; display_qty = o.display_qty
       ; ord_type = o.ord_type
       ; time_in_force = o.tif
       ; exec_restatement_reason = reason
       })
;;

let report_modify t o =
  report
    t
    o.account
    (Execution_report_modify
       { common = common t o
       ; price = o.price
       ; stop_px = None
       ; order_qty = o.order_qty
       ; cum_qty = o.cum_qty
       ; leaves_qty = leaves o
       ; min_qty = o.min_qty
       ; display_qty = o.display_qty
       ; ord_type = o.ord_type
       ; time_in_force = o.tif
       })
;;

let report_trade t o ~px ~qty ~aggressor ~md_trade_entry_id =
  let side_trade_id = t.next_side_trade_id in
  t.next_side_trade_id <- side_trade_id + 1;
  report
    t
    o.account
    (Execution_report_trade_outright
       { common = common t o
       ; last_px = px
       ; price = o.price
       ; sec_exec_id = side_trade_id
       ; order_qty = o.order_qty
       ; last_qty = qty
       ; cum_qty = o.cum_qty
       ; md_trade_entry_id
       ; side_trade_id
       ; leaves_qty = leaves o
       ; trade_date = None
       ; ord_status = (if leaves o = 0 then Filled else Partially_filled)
       ; ord_type = o.ord_type
       ; time_in_force = o.tif
       ; aggressor
       ; fills = []
       })
;;

let reject_new t ~account (r : Ilink3.New_order_single.t) ~reason ~text =
  report
    t
    account
    (Execution_report_reject
       { common =
           { seq_num = 0
           ; uuid = 0
           ; exec_id = next_exec_id t
           ; sender_id = r.sender_id
           ; cl_ord_id = r.cl_ord_id
           ; party_details_list_req_id = r.party_details_list_req_id
           ; order_id = 0
           ; transact_time = t.now
           ; sending_time = t.now
           ; order_request_id = r.order_request_id
           ; location = r.location
           ; security_id = r.security_id
           ; side = r.side
           ; manual = r.manual
           ; poss_retrans = false
           }
       ; text
       ; price = r.price
       ; stop_px = r.stop_px
       ; order_qty = r.order_qty
       ; min_qty = r.min_qty
       ; display_qty = r.display_qty
       ; ord_rej_reason = reason
       ; ord_type = r.ord_type
       ; time_in_force = r.time_in_force
       })
;;

let cancel_reject
  t
  ~account
  ~replace
  ~sender_id
  ~cl_ord_id
  ~party
  ~order_id
  ~order_request_id
  ~location
  ~manual
  ~reason
  ~text
  =
  let r : Ilink3.Cancel_reject.t =
    { seq_num = 0
    ; uuid = 0
    ; text
    ; exec_id = next_exec_id t
    ; sender_id
    ; cl_ord_id
    ; party_details_list_req_id = party
    ; order_id
    ; transact_time = t.now
    ; sending_time = t.now
    ; order_request_id
    ; location
    ; cxl_rej_reason = reason
    ; manual
    ; poss_retrans = false
    }
  in
  report
    t
    account
    (if replace then Order_cancel_replace_reject r else Order_cancel_reject r)
;;

(* {2 Order book bookkeeping} *)

let forget t o =
  Hashtbl.remove t.orders o.order_id;
  Option.iter o.engine_oid ~f:(Hashtbl.remove t.by_engine);
  o.engine_oid <- None
;;

let mark_book t o = Hash_set.add t.changed_books o.product.security_id

let flush_group t =
  Option.iter t.group ~f:(fun g ->
    t.group <- None;
    emit
      t
      (Trade
         { security_id = g.g_security_id
         ; px = g.g_px
         ; size = g.g_size
         ; aggressor = Some g.g_aggressor
         ; md_trade_entry_id = g.g_id
         ; orders = List.rev g.g_orders
         ; time = t.now
         }))
;;

let next_trade_entry_id t =
  let id = t.next_trade_entry_id in
  t.next_trade_entry_id <- id + 1;
  id
;;

(* The trade summary entry a fill belongs to. *)
let group_for t (fill : Order_book.Fill.t) ~security_id ~aggressor =
  match t.group with
  | Some g when Order_id.equal g.g_taker fill.taker_oid && Price.equal g.g_px fill.px -> g
  | _ ->
    flush_group t;
    let g =
      { g_security_id = security_id
      ; g_taker = fill.taker_oid
      ; g_px = fill.px
      ; g_aggressor = aggressor
      ; g_id = next_trade_entry_id t
      ; g_size = 0
      ; g_orders = []
      }
    in
    t.group <- Some g;
    g
;;

let contracts size = Size.to_units size / Size.units_per_one

let venue_order t oid =
  Option.bind (Hashtbl.find t.by_engine oid) ~f:(Hashtbl.find t.orders)
;;

(* Places the next slice of [o] in the engine: everything for a plain order, the display
   quantity for an iceberg. Returns the engine status. *)
let place_slice t o =
  let qty = Option.value_map o.display_qty ~default:(leaves o) ~f:(Int.min (leaves o)) in
  let tif : Order_types.Tif.t = if is_ioc o then Ioc else Gtc in
  let request =
    Order_types.to_order_request
      ~client_order_id:(Client_order_id.of_string (Int.to_string o.order_id))
      (Order_types.create ~limit:o.price ~tif (order_side o.side) ~qty)
      ~symbol:(Products.engine_symbol o.product)
    |> Or_error.ok_exn
  in
  o.slice_left <- qty;
  let oid, status = Exchange.place_with_order_id t.exchange o.account request in
  o.engine_oid <- Some oid;
  Hashtbl.set t.by_engine ~key:oid ~data:o.order_id;
  mark_book t o;
  status
;;

let on_fill t (fill : Order_book.Fill.t) =
  let product = product_of_symbol fill.symbol in
  let security_id = product.security_id in
  Hashtbl.set t.last_trades ~key:security_id ~data:fill.px;
  let qty = contracts fill.sz in
  (* A refreshed iceberg slice that crosses a resting remainder is still the passive side:
     the other order was aggressing when the slice ran out. *)
  let swapped = Hash_set.mem t.refill_slices fill.taker_oid in
  let aggressor_side =
    ilink_side (if swapped then Order_book.Fill.maker_side fill else fill.taker_side)
  in
  let g = group_for t fill ~security_id ~aggressor:aggressor_side in
  let public_id oid =
    match venue_order t oid with
    | Some o -> o.order_id
    | None -> Order_id.to_int_exn oid
  in
  g.g_size <- g.g_size + qty;
  g.g_orders <- (public_id fill.maker_oid, qty) :: g.g_orders;
  let fill_side oid ~aggressor =
    Option.iter (venue_order t oid) ~f:(fun o ->
      o.cum_qty <- o.cum_qty + qty;
      o.slice_left <- o.slice_left - qty;
      report_trade t o ~px:fill.px ~qty ~aggressor ~md_trade_entry_id:g.g_id;
      if leaves o = 0
      then forget t o
      else if o.slice_left = 0 && not (is_ioc o)
      then (
        (* an iceberg slice is gone: refresh it at the back of the queue *)
        Option.iter o.engine_oid ~f:(Hashtbl.remove t.by_engine);
        o.engine_oid <- None;
        Queue.enqueue t.refills o))
  in
  fill_side fill.maker_oid ~aggressor:swapped;
  fill_side fill.taker_oid ~aggressor:(not swapped)
;;

let on_event t (e : Event.t) =
  Option.iter (Event.book_symbol e) ~f:(fun s ->
    Hash_set.add t.changed_books (product_of_symbol s).security_id);
  match e with
  | Fill { fill; _ } -> on_fill t fill
  | Order_cancelled { order; reason; _ } ->
    flush_group t;
    (match venue_order t order.order_id with
     | None -> ()
     | Some o ->
       (match reason with
        | User | Modified -> ()
        | Ioc_unfilled ->
          report_elimination t o;
          forget t o
        | Self_trade ->
          report_cancel t o ~reason:smp_newest_cancelled;
          forget t o
        | Liquidated ->
          report_cancel t o;
          forget t o))
  | Order_accepted _ | Order_rejected _ | Order_filled _ | Liquidation _ | Funding _ ->
    flush_group t
;;

let rec process_events t =
  let events = Exchange.drain_events t.exchange in
  List.iter events ~f:(on_event t);
  flush_group t;
  if not (Queue.is_empty t.refills)
  then (
    let pending = Queue.to_list t.refills in
    Queue.clear t.refills;
    List.iter pending ~f:(fun o ->
      ignore (place_slice t o : Exchange_core.Order_status.t);
      Option.iter o.engine_oid ~f:(Hash_set.add t.refill_slices)));
  if not (List.is_empty events)
  then process_events t
  else (
    Hash_set.clear t.refill_slices;
    let changed = Hash_set.to_list t.changed_books |> List.sort ~compare:Int.compare in
    Hash_set.clear t.changed_books;
    List.iter changed ~f:(fun security_id -> emit t (Book_changed { security_id })))
;;

let sync t = process_events t

(* Sends a (non-queued) order to the engine and reports what happens. *)
let execute t o =
  match place_slice t o with
  | Rejected No_liquidity ->
    (* fill and kill with nothing to fill *)
    report_elimination t o;
    forget t o;
    process_events t
  | Rejected Self_trade ->
    report_cancel t o ~reason:smp_newest_cancelled;
    forget t o;
    process_events t
  | Rejected _ ->
    report_elimination t o;
    forget t o;
    process_events t
  | Resting _ | Filled _ -> process_events t
;;

(* Quantity available to an aggressing order at or better than [price]. *)
let available t o =
  match Exchange.l2 ~n_levels:1000 t.exchange (Products.engine_symbol o.product) with
  | None -> 0
  | Some l2 ->
    let levels, better =
      match o.side with
      | Buy -> l2.asks, fun (px : Price.t) -> Price.(px <= o.price)
      | Sell -> l2.bids, fun (px : Price.t) -> Price.(px >= o.price)
    in
    List.take_while levels ~f:(fun (l : Exchange_core.L2.Level.t) -> better l.px)
    |> List.sum (module Int) ~f:(fun (l : Exchange_core.L2.Level.t) -> contracts l.sz)
;;

(* {2 Requests} *)

let best_opposite t product (side : Ilink3.Side.t) =
  Option.bind
    (Exchange.l2 t.exchange (Products.engine_symbol product))
    ~f:(fun l2 ->
      match side with
      | Buy -> Exchange_core.L2.best_ask l2
      | Sell -> Exchange_core.L2.best_bid l2)
  |> Option.map ~f:(fun (l : Exchange_core.L2.Level.t) -> l.px)
;;

(* Market with protection: a limit at the best opposite price plus the protection range,
   inside the price limits. *)
let protection_price t product side =
  let low, high = price_limits t ~security_id:product.Products.security_id in
  Option.map (best_opposite t product side) ~f:(fun best ->
    match (side : Ilink3.Side.t) with
    | Buy -> Price.min high Price.(best + product.protection)
    | Sell -> Price.max low Price.(best - product.protection))
;;

let check cond ~reason ~text = if cond then Ok () else Error (reason, text)

let validate_price t product price =
  let open Result.Let_syntax in
  let low, high = price_limits t ~security_id:product.Products.security_id in
  let%bind () =
    check
      (Price.is_positive price
       && Equities_core.Tick_size.is_on_tick ~tick:(Products.tick product) price)
      ~reason:Reason.invalid_price_increment
      ~text:
        (sprintf
           "price %s is not a multiple of the tick %s"
           (Price.to_string price)
           (Price.to_string (Products.tick product)))
  in
  check
    Price.(price >= low && price <= high)
    ~reason:Reason.exceeds_limit
    ~text:
      (sprintf
         "price %s outside the price limits %s to %s"
         (Price.to_string price)
         (Price.to_string low)
         (Price.to_string high))
;;

let validate_new t (r : Ilink3.New_order_single.t) =
  let open Result.Let_syntax in
  let%bind product =
    Result.of_option
      (Products.find_security_id r.security_id)
      ~error:(Reason.unknown_symbol, sprintf "unknown security id %d" r.security_id)
  in
  let phase = phase t ~security_id:product.security_id in
  let%bind () =
    match phase with
    | Open | Pre_open -> Ok ()
    | Halted -> Error (Reason.exchange_closed, "trading is halted")
    | Closed -> Error (Reason.exchange_closed, "market is closed")
  in
  let%bind () =
    match r.ord_type with
    | Limit | Market_with_protection -> Ok ()
    | Stop_with_protection | Stop_limit | Market_with_leftover_as_limit ->
      Error (Reason.unsupported, "order type not supported by the simulator")
  in
  let%bind (tif : Order_types.Tif.t) =
    match r.time_in_force with
    | Day -> Ok Order_types.Tif.Day
    | Gtc -> Ok Order_types.Tif.Gtc
    | Fak -> Ok Order_types.Tif.Ioc
    | Fok -> Ok Order_types.Tif.Fok
    | Gtd -> Error (Reason.unsupported, "good till date is not supported")
  in
  let%bind () =
    check
      (not
         (Phase.equal phase Pre_open
          && (Ilink3.Ord_type.equal r.ord_type Market_with_protection
              || Order_types.Tif.(equal tif Ioc || equal tif Fok))))
      ~reason:Reason.unsupported
      ~text:"market, FAK and FOK orders are not accepted in pre-open"
  in
  let%bind () =
    Equities_core.Lot.Contract.validate_quantity ~max:product.max_trade_vol r.order_qty
    |> Result.map_error ~f:(fun e -> Reason.incorrect_quantity, Error.to_string_hum e)
  in
  let%bind () =
    check
      (Option.is_none r.min_qty || Order_types.Tif.equal tif Ioc)
      ~reason:Reason.unsupported
      ~text:"minimum quantity is only valid on fill and kill orders"
  in
  let display_qty = Option.filter r.display_qty ~f:(fun d -> d < r.order_qty) in
  let%bind price =
    match r.ord_type, r.price with
    | Market_with_protection, _ ->
      Result.of_option
        (protection_price t product r.side)
        ~error:(Reason.other, "no opposite side to protect a market order against")
    | _, Some price ->
      let%map () = validate_price t product price in
      price
    | _, None -> Error (Reason.other, "limit order without a price")
  in
  let%bind () =
    Order_types.validate
      ~round_lot:1
      { (Order_types.create ~limit:price ~tif (order_side r.side) ~qty:r.order_qty) with
        display =
          (match display_qty with
           | None -> Visible
           | Some display_qty -> Reserve { display_qty })
      ; min_qty = r.min_qty
      }
    |> Result.map_error ~f:(fun e -> Reason.unsupported, Error.to_string_hum e)
  in
  return (product, price, display_qty)
;;

let new_order t ~account (r : Ilink3.New_order_single.t) =
  fund t account;
  match validate_new t r with
  | Error (reason, text) -> reject_new t ~account r ~reason ~text
  | Ok (product, price, display_qty) ->
    let o =
      { order_id = t.next_order_id
      ; account
      ; product
      ; side = r.side
      ; ord_type = r.ord_type
      ; tif = r.time_in_force
      ; min_qty = r.min_qty
      ; manual = r.manual
      ; arrival = t.next_arrival
      ; display_qty
      ; price
      ; order_qty = r.order_qty
      ; cum_qty = 0
      ; cl_ord_id = r.cl_ord_id
      ; sender_id = r.sender_id
      ; location = r.location
      ; party = r.party_details_list_req_id
      ; order_request_id = r.order_request_id
      ; engine_oid = None
      ; slice_left = 0
      ; queued = false
      }
    in
    t.next_order_id <- t.next_order_id + 1;
    t.next_arrival <- t.next_arrival + 1;
    Hashtbl.set t.orders ~key:o.order_id ~data:o;
    report_new t o;
    (match phase t ~security_id:product.security_id with
     | Pre_open -> o.queued <- true
     | Open | Halted | Closed ->
       let required =
         match o.tif, o.min_qty with
         | Fok, _ -> Some o.order_qty
         | _, Some min_qty -> Some min_qty
         | _, None -> None
       in
       (match required with
        | Some required when available t o < required ->
          report_elimination t o;
          forget t o
        | _ -> execute t o))
;;

let find_order t ~account order_id =
  Option.filter (Hashtbl.find t.orders order_id) ~f:(fun o ->
    Account.equal o.account account)
;;

let cancel_order ?reason t o =
  let gone =
    o.queued
    ||
    match o.engine_oid with
    | None -> true
    | Some oid ->
      (match
         Exchange.cancel
           t.exchange
           o.account
           ~symbol:(Products.engine_symbol o.product)
           oid
       with
       | Cancelled -> true
       | Not_found -> false)
  in
  if gone
  then (
    report_cancel ?reason t o;
    forget t o;
    mark_book t o;
    process_events t);
  gone
;;

let cancel t ~account (r : Ilink3.Order_cancel_request.t) =
  fund t account;
  let reject ~reason ~text =
    cancel_reject
      t
      ~account
      ~replace:false
      ~sender_id:r.sender_id
      ~cl_ord_id:r.cl_ord_id
      ~party:r.party_details_list_req_id
      ~order_id:r.order_id
      ~order_request_id:r.order_request_id
      ~location:r.location
      ~manual:r.manual
      ~reason
      ~text
  in
  match find_order t ~account r.order_id with
  | None -> reject ~reason:Reason.cxl_unknown_order ~text:"unknown order"
  | Some o ->
    o.cl_ord_id <- r.cl_ord_id;
    o.sender_id <- r.sender_id;
    o.order_request_id <- r.order_request_id;
    o.location <- r.location;
    if not (cancel_order t o)
    then reject ~reason:Reason.too_late_to_cancel ~text:"order is no longer working"
;;

let replace t ~account (r : Ilink3.Order_cancel_replace_request.t) =
  fund t account;
  let reject ~reason ~text =
    cancel_reject
      t
      ~account
      ~replace:true
      ~sender_id:r.sender_id
      ~cl_ord_id:r.cl_ord_id
      ~party:r.party_details_list_req_id
      ~order_id:r.order_id
      ~order_request_id:r.order_request_id
      ~location:r.location
      ~manual:r.manual
      ~reason
      ~text
  in
  match find_order t ~account r.order_id with
  | None -> reject ~reason:Reason.cxl_unknown_order ~text:"unknown order"
  | Some o ->
    let validated =
      let open Result.Let_syntax in
      let%bind () =
        check
          (o.product.security_id = r.security_id && Ilink3.Side.equal o.side r.side)
          ~reason:Reason.other
          ~text:"security and side cannot be modified"
      in
      let%bind () =
        match phase t ~security_id:o.product.security_id with
        | Open | Pre_open -> Ok ()
        | Halted | Closed -> Error (Reason.other, "market is not open")
      in
      let%bind price =
        match r.price with
        | None -> Ok o.price
        | Some price ->
          let%map () = validate_price t o.product price in
          price
      in
      let%bind () =
        check
          (r.order_qty > o.cum_qty && r.order_qty <= o.product.max_trade_vol)
          ~reason:Reason.other
          ~text:
            (sprintf
               "new quantity %d must exceed the filled quantity %d"
               r.order_qty
               o.cum_qty)
      in
      return price
    in
    (match validated with
     | Error (reason, text) -> reject ~reason ~text
     | Ok price ->
       o.price <- price;
       o.order_qty <- r.order_qty;
       o.display_qty <- Option.filter r.display_qty ~f:(fun d -> d < r.order_qty);
       o.cl_ord_id <- r.cl_ord_id;
       o.sender_id <- r.sender_id;
       o.order_request_id <- r.order_request_id;
       o.location <- r.location;
       if o.queued
       then report_modify t o
       else (
         (match o.engine_oid with
          | None -> ()
          | Some oid ->
            ignore
              (Exchange.cancel
                 t.exchange
                 o.account
                 ~symbol:(Products.engine_symbol o.product)
                 oid
               : Exchange_core.Cancel_status.t);
            Hashtbl.remove t.by_engine oid;
            o.engine_oid <- None);
         report_modify t o;
         execute t o))
;;

let cancel_on_disconnect t ~account =
  Hashtbl.data t.orders
  |> List.filter ~f:(fun o -> Account.equal o.account account)
  |> List.sort ~compare:(fun a b -> Int.compare a.order_id b.order_id)
  |> List.iter ~f:(fun o ->
    ignore (cancel_order t o ~reason:cancel_on_disconnect_reason : bool))
;;

(* {2 Phases} *)

let settle t product =
  let security_id = product.Products.security_id in
  let px =
    match Hashtbl.find t.last_trades security_id with
    | Some px -> px
    | None ->
      Option.value
        (Exchange.mark t.exchange (Products.engine_symbol product))
        ~default:(reference t ~security_id)
  in
  let px = Equities_core.Tick_size.round ~tick:(Products.tick product) `Nearest px in
  Hashtbl.set t.references ~key:security_id ~data:px;
  emit t (Settlement { security_id; px; time = t.now })
;;

let product_orders t product =
  Hashtbl.data t.orders
  |> List.filter ~f:(fun o -> o.product.security_id = product.Products.security_id)
  |> List.sort ~compare:(fun a b -> Int.compare a.arrival b.arrival)
;;

let open_market t product ~from =
  match (from : Phase.t) with
  | Pre_open ->
    List.iter (product_orders t product) ~f:(fun o ->
      if o.queued
      then (
        o.queued <- false;
        execute t o))
  | Open | Halted | Closed -> ()
;;

let set_phase t ~security_id next =
  match Products.find_security_id security_id with
  | None -> ()
  | Some product ->
    let from = phase t ~security_id in
    if not (Phase.equal from next)
    then (
      Hashtbl.set t.phases ~key:security_id ~data:next;
      emit t (Phase_changed { security_id; phase = next; time = t.now });
      match next with
      | Closed ->
        List.iter (product_orders t product) ~f:(fun o ->
          if Ilink3.Time_in_force.equal o.tif Day
          then (
            (match o.engine_oid with
             | Some oid ->
               ignore
                 (Exchange.cancel
                    t.exchange
                    o.account
                    ~symbol:(Products.engine_symbol product)
                    oid
                  : Exchange_core.Cancel_status.t)
             | None -> ());
            report_elimination t o;
            forget t o));
        process_events t;
        settle t product
      | Open -> open_market t product ~from
      | Pre_open | Halted -> ())
;;

module Order_view = struct
  type t =
    { order_id : int
    ; cl_ord_id : string
    ; security_id : int
    ; side : Ilink3.Side.t
    ; price : Price.t
    ; order_qty : int
    ; cum_qty : int
    ; display_qty : int option
    ; queued : bool
    }
  [@@deriving sexp_of]
end

let orders t ~account =
  Hashtbl.data t.orders
  |> List.filter ~f:(fun o -> Account.equal o.account account)
  |> List.sort ~compare:(fun a b -> Int.compare a.order_id b.order_id)
  |> List.map ~f:(fun o : Order_view.t ->
    { order_id = o.order_id
    ; cl_ord_id = o.cl_ord_id
    ; security_id = o.product.security_id
    ; side = o.side
    ; price = o.price
    ; order_qty = o.order_qty
    ; cum_qty = o.cum_qty
    ; display_qty = o.display_qty
    ; queued = o.queued
    })
;;
