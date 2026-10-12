open! Core
open Cme_sim
module Price = Exchange_core.Price
module Market_sim = Exchange_core_engine.Market_sim
module Path = Price_anchor.Path

let show_spec parse s =
  match parse s with
  | Ok (spec : Anchoring.Spec.t) ->
    print_s
      [%message
        (Products.symbol spec.product : string) (spec.source : Anchoring.Spec.source)]
  | Error e -> print_s [%message "error" (e : Error.t)]
;;

let%expect_test "flag parsing" =
  show_spec Anchoring.Spec.series_of_string "data/es.txt=ESZ6";
  show_spec Anchoring.Spec.fixed_of_string "6800=ESZ6";
  show_spec Anchoring.Spec.fixed_of_string "62.5,-3=CLZ6";
  show_spec Anchoring.Spec.scaled_of_string "binance:BTCUSDT=GCZ6,0.04";
  show_spec Anchoring.Spec.fixed_of_string "6800=XXZ6";
  show_spec Anchoring.Spec.fixed_of_string "-1=ESZ6";
  show_spec Anchoring.Spec.scaled_of_string "binance:BTCUSDT=GCZ6";
  [%expect
    {|
    (("Products.symbol spec.product" ESZ6)
     (spec.source (Series_file data/es.txt)))
    (("Products.symbol spec.product" ESZ6)
     (spec.source (Fixed (price 6800) (drift_bps_per_day 0))))
    (("Products.symbol spec.product" CLZ6)
     (spec.source (Fixed (price 62.5) (drift_bps_per_day -3))))
    (("Products.symbol spec.product" GCZ6)
     (spec.source
      (Scaled (reference ((venue Binance) (symbol BTCUSDT))) (factor 0.04))))
    (error (e ("unknown symbol" (symbol XXZ6))))
    (error (e ("expected a positive price" -1)))
    (error (e (expected (what SIM_SYMBOL,FACTOR) (got GCZ6))))
    |}]
;;

let es = Option.value_exn (Products.find_symbol "ESZ6")
let start = Time_ns.of_int_ns_since_epoch 1_800_000_000_000_000_000
let since_start s = Time_ns.add start (Time_ns.Span.of_sec s)

(* A venue at [start] with one noise simulator and one anchor per product; [show] steps
   the anchors at [secs] after the start and prints their targets. *)
let anchored references =
  let venue = Venue.create () in
  Venue.set_time venue (Time_ns.to_int_ns_since_epoch start);
  let anchors =
    List.mapi references ~f:(fun i ((p : Products.t), reference) ->
      let symbol = Products.engine_symbol p in
      let sim =
        Market_sim.create
          { Market_sim.Config.default with
            seed = i
          ; symbols = [ symbol ]
          ; initial_prices = [ symbol, p.reference_price ]
          }
          (Venue.exchange venue)
      in
      p, reference, sim)
  in
  let anchoring = Anchoring.create venue anchors in
  fun symbol secs ->
    let p = Option.value_exn (Products.find_symbol symbol) in
    Venue.set_time venue (Time_ns.to_int_ns_since_epoch (since_start secs));
    Anchoring.step anchoring p;
    printf
      "%s +%gs %s\n"
      symbol
      secs
      (Option.value_map (Anchoring.target anchoring p) ~f:Price.to_string ~default:"-")
;;

let unscaled path = { Anchoring.Reference.path = ok_exn path; price_factor = 1. }

let%expect_test "targets: series followed in real time, fixed price with drift" =
  let t0 = Time_ns.of_int_ns_since_epoch 1_700_000_000_000_000_000 in
  let point s px = Time_ns.add t0 (Time_ns.Span.of_sec s), Price.of_string px in
  let es_path = Path.series [ point 0. "6750.1"; point 10. "6760"; point 20. "6755.4" ] in
  let show = anchored [ es, unscaled es_path ] in
  List.iter [ 0.; 9.9; 10.; 15.; 25.; 1000. ] ~f:(show "ESZ6");
  show "GCZ6" 0.;
  (* A second venue: the clock only moves forward. *)
  let show =
    anchored
      [ ( Option.value_exn (Products.find_symbol "CLZ6")
        , unscaled (Path.fixed ~drift_bps_per_day:100. (Price.of_string "62.5")) )
      ]
  in
  List.iter [ 0.; 86_400.; 2. *. 86_400. ] ~f:(show "CLZ6");
  [%expect
    {|
    ESZ6 +0s 6750
    ESZ6 +9.9s 6750
    ESZ6 +10s 6760
    ESZ6 +15s 6760
    ESZ6 +25s 6755.5
    ESZ6 +1000s 6755.5
    GCZ6 +0s -
    CLZ6 +0s 62.5
    CLZ6 +86400s 63.13
    CLZ6 +172800s 63.76
    |}]
;;

module Scaled = struct
  open! Async

  let%expect_test "scaled: a recorded instrument's prices times a factor" =
    let instrument = Market_data.Instrument.create Binance "BTCUSDT" in
    let t0 = Time_ns.of_string_with_utc_offset "2026-10-09T08:00:00Z" in
    let ms n = Time_ns.add t0 (Time_ns.Span.of_int_ms n) in
    let level px sz =
      { Market_data.L2_update.Level.price = Price.of_string px
      ; size = Market_data.Size.of_string sz
      }
    in
    let dir = "anchor-scaled-test" in
    let writer = Market_data.Event_log.Writer.create ~dir () in
    List.iter
      ~f:(Market_data.Event_log.Writer.append writer)
      [ Market_data.Event.L2
          { instrument
          ; exch_ts = ms 0
          ; recv_ts = ms 0
          ; kind = Snapshot
          ; bids = [ level "100000" "1" ]
          ; asks = [ level "100002" "1" ]
          }
      ; Market_data.Event.Trade
          { instrument
          ; exch_ts = ms 1000
          ; recv_ts = ms 1000
          ; price = Price.of_string "100010"
          ; size = Market_data.Size.of_string "0.1"
          ; side = Buy
          ; trade_id = "1"
          }
      ; Market_data.Event.L2
          { instrument
          ; exch_ts = ms 2000
          ; recv_ts = ms 2000
          ; kind = Delta
          ; bids = [ level "100020" "1" ]
          ; asks = [ level "100002" "0"; level "100030" "2" ]
          }
      ];
    let%bind () = Market_data.Event_log.Writer.close writer in
    let date = Date.of_string "2026-10-09" in
    let spec = ok_exn (Anchoring.Spec.scaled_of_string "binance:BTCUSDT=GCZ6,0.04") in
    let%bind loaded = Anchoring.load spec in
    print_s [%sexp (Result.is_error loaded : bool)];
    let%bind product, reference =
      Anchoring.load ~norm:{ dir; from = date; to_ = date } spec >>| ok_exn
    in
    let show = anchored [ product, reference ] in
    List.iter [ 0.; 1.; 2.; 60. ] ~f:(show "GCZ6");
    let%map _ = Unix.system (sprintf "rm -rf %s" dir) in
    [%expect
      {|
      true
      GCZ6 +0s 4000
      GCZ6 +1s 4000
      GCZ6 +2s 4001
      GCZ6 +60s 4001
      |}]
  ;;
end
