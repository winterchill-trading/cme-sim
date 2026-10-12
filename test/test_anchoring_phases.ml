(* The anchor on a synthetic clock, driven the way [Server] drives it: every 100 ms the
   anchor steps, then the noise traders tick while the product is open. *)

open! Core
open Cme_sim
module Price = Exchange_core.Price
module Market_sim = Exchange_core_engine.Market_sim

let es = Option.value_exn (Products.find_symbol "ESZ6")
let security_id = es.security_id
let symbol = Products.engine_symbol es
let tick = Time_ns.Span.of_int_ms 100

type h =
  { venue : Venue.t
  ; sim : Market_sim.t
  ; anchoring : Anchoring.t
  ; mutable now : Time_ns.t
  ; mutable trades : Price.t list (** newest first *)
  ; mutable settlement : Price.t option
  }

let create () =
  let now = Time_ns.of_string_with_utc_offset "2026-10-12T13:30:00Z" in
  let venue = Venue.create () in
  Venue.set_time venue (Time_ns.to_int_ns_since_epoch now);
  let sim =
    Market_sim.create
      { Market_sim.Config.default with
        seed = 3
      ; symbols = [ symbol ]
      ; initial_prices = [ symbol, es.reference_price ]
      ; vol_bps = 0.
      ; min_notional = 100.
      ; max_notional = 500.
      }
      (Venue.exchange venue)
  in
  let anchoring =
    Anchoring.create
      ~start:now
      [ es, Fixed { price = Price.of_string "6800"; drift_bps_per_day = 0. } ]
  in
  { venue; sim; anchoring; now; trades = []; settlement = None }
;;

let drain h =
  List.iter (Venue.drain h.venue) ~f:(function
    | Trade { px; _ } -> h.trades <- px :: h.trades
    | Settlement { px; _ } -> h.settlement <- Some px
    | _ -> ())
;;

let run h ~secs =
  for _ = 1 to Float.iround_nearest_exn (secs *. 10.) do
    h.now <- Time_ns.add h.now tick;
    Venue.set_time h.venue (Time_ns.to_int_ns_since_epoch h.now);
    Anchoring.step h.anchoring es h.sim ~venue:h.venue ~now:h.now;
    (match Venue.phase h.venue ~security_id with
     | Open -> Market_sim.tick h.sim
     | Pre_open | Halted | Closed -> ());
    Venue.sync h.venue;
    drain h
  done
;;

let set_phase h phase =
  Venue.set_phase h.venue ~security_id phase;
  drain h
;;

let show h label =
  let price = Option.value_map ~default:"-" ~f:Price.to_string in
  let stats = Option.value_exn (Anchoring.stats h.anchoring es) in
  printf
    "%-26s %-8s reference %-8s mark %-8s last %-8s pulls %3d waits %3d restarts %d\n"
    label
    (Sexp.to_string [%sexp (Venue.phase h.venue ~security_id : Venue.Phase.t)])
    (price (Market_sim.reference_price h.sim symbol))
    (price (Exchange_core_engine.Exchange.mark (Venue.exchange h.venue) symbol))
    (Price.to_string (Venue.last_price h.venue ~security_id))
    stats.pulls
    stats.waits
    stats.restarts
;;

let%expect_test "the anchor waits through pre-open and a halt, and resumes at the open" =
  let h = create () in
  set_phase h Pre_open;
  run h ~secs:2.;
  show h "pre-open 2 s";
  set_phase h Open;
  show h "opening auction";
  run h ~secs:5.;
  show h "open 5 s (5 half-lives)";
  set_phase h Halted;
  let before = Market_sim.reference_price h.sim symbol in
  run h ~secs:3.;
  show h "halted 3 s";
  print_s
    [%message
      "reference frozen while halted"
        ~_:
          ([%equal: Price.t option] before (Market_sim.reference_price h.sim symbol)
           : bool)];
  set_phase h Pre_open;
  run h ~secs:1.;
  set_phase h Open;
  run h ~secs:0.1;
  show h "reopened";
  run h ~secs:5.;
  show h "open 5 s more";
  [%expect
    {|
    pre-open 2 s               Pre_open reference 6700     mark -        last 6700     pulls   0 waits  20 restarts 0
    opening auction            Open     reference 6700     mark -        last 6700     pulls   0 waits  20 restarts 0
    open 5 s (5 half-lives)    Open     reference 6797     mark 6797     last 6794.5   pulls  50 waits  20 restarts 1
    halted 3 s                 Halted   reference 6797     mark 6797     last 6794.5   pulls  50 waits  50 restarts 1
    ("reference frozen while halted" true)
    reopened                   Open     reference 6794.75  mark 6794.75  last 6794.5   pulls  51 waits  60 restarts 2
    open 5 s more              Open     reference 6799.75  mark 6799.75  last 6802.25  pulls 101 waits  60 restarts 2
    |}]
;;

let%expect_test "price limits keep coming from the simulator's own trades" =
  let h = create () in
  run h ~secs:10.;
  let last_trade = List.hd_exn h.trades in
  set_phase h Closed;
  let settlement = h.settlement in
  let low, high = Venue.price_limits h.venue ~security_id in
  let expected_low, expected_high = Products.price_limits es ~reference:last_trade in
  print_s
    [%message
      ""
        ~trades:(List.length h.trades : int)
        (last_trade : Price.t)
        (settlement : Price.t option)
        ~target:(Anchoring.target h.anchoring es ~now:h.now : Price.t option)
        (low : Price.t)
        (high : Price.t)
        ~limits_from_last_trade:
          (Price.equal low expected_low && Price.equal high expected_high : bool)];
  [%expect
    {|
    ((trades 62) (last_trade 6799.25) (settlement (6799.25)) (target (6800))
     (low 6323.5) (high 7275) (limits_from_last_trade true))
    |}]
;;
