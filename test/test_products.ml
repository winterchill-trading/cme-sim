open! Core
open Cme_sim
module Price = Exchange_core.Price

let%expect_test "instruments" =
  List.iter Products.all ~f:(fun p ->
    let low, high = Products.price_limits p ~reference:p.reference_price in
    printf
      "%s id %d tick %s x%d ref %s limits %s..%s last trade %s\n"
      (Products.symbol p)
      p.security_id
      (Price.to_string (Products.tick p))
      (Products.multiplier p)
      (Price.to_string p.reference_price)
      (Price.to_string low)
      (Price.to_string high)
      (Date.to_string (Products.last_trade_date p)));
  [%expect
    {|
    ESZ6 id 1001 tick 0.25 x50 ref 6700 limits 6231..7169 last trade 2026-12-18
    NQZ6 id 1002 tick 0.25 x20 ref 24500 limits 22785..26215 last trade 2026-12-18
    CLZ6 id 1003 tick 0.01 x1000 ref 62.5 limits 53.13..71.87 last trade 2026-11-20
    GCZ6 id 1004 tick 0.1 x100 ref 4000 limits 3600..4400 last trade 2026-12-29
    |}];
  print_s
    [%sexp
      (Option.map (Products.find_symbol "clz6") ~f:(fun p -> p.security_id) : int option)];
  [%expect {| (1003) |}]
;;
