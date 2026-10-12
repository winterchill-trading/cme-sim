open! Core
open Cme_sim
module Price = Exchange_core.Price

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
let at s = Time_ns.add start (Time_ns.Span.of_sec s)

let%expect_test "targets: series followed in real time, fixed price with drift" =
  let t0 = Time_ns.of_int_ns_since_epoch 1_700_000_000_000_000_000 in
  let point s px = Time_ns.add t0 (Time_ns.Span.of_sec s), Price.of_string px in
  let anchoring =
    Anchoring.create
      ~start
      [ es, Series [| point 0. "6750.1"; point 10. "6760"; point 20. "6755.4" |]
      ; ( Option.value_exn (Products.find_symbol "CLZ6")
        , Fixed { price = Price.of_string "62.5"; drift_bps_per_day = 100. } )
      ]
  in
  let show symbol secs =
    let p = Option.value_exn (Products.find_symbol symbol) in
    printf
      "%s +%gs %s\n"
      symbol
      secs
      (Option.value_map
         (Anchoring.target anchoring p ~now:(at secs))
         ~f:Price.to_string
         ~default:"-")
  in
  List.iter [ 0.; 9.9; 10.; 15.; 25.; 1000. ] ~f:(show "ESZ6");
  List.iter [ 0.; 86_400.; 2. *. 86_400. ] ~f:(show "CLZ6");
  show "GCZ6" 0.;
  [%expect
    {|
    ESZ6 +0s 6750
    ESZ6 +9.9s 6750
    ESZ6 +10s 6760
    ESZ6 +15s 6760
    ESZ6 +25s 6755.5
    ESZ6 +1000s 6755.5
    CLZ6 +0s 62.5
    CLZ6 +86400s 63.13
    CLZ6 +172800s 63.76
    GCZ6 +0s -
    |}]
;;
