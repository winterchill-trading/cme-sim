open! Core
open Cme_sim
open Venue_helpers

let%expect_test "a resting order is hit" =
  let v = Venue.create () in
  Venue.new_order v ~account:alice (order Buy 3 ~price:"6700.25" ~cl_ord_id:"A1");
  show v;
  [%expect {|
    001/ALC New A1 Buy 3 @ 6700.25
      book 1001
    |}];
  Venue.new_order v ~account:bob (order Sell 2 ~price:"6700" ~cl_ord_id:"B1");
  show v;
  [%expect
    {|
    002/BOB New B1 Sell 2 @ 6700
    001/ALC Trade A1 2 @ 6700.25 cum 2 leaves 1
    002/BOB Trade B1 2 @ 6700.25 cum 2 leaves 0 aggressor
      trade 1001: 2 @ 6700.25 aggressor (Sell) orders 1
      book 1001
    |}];
  print_s [%sexp (Venue.orders v ~account:alice : Venue.Order_view.t list)];
  [%expect
    {|
    (((order_id 700000000001) (cl_ord_id A1) (security_id 1001) (side Buy)
      (price 6700.25) (order_qty 3) (cum_qty 2) (display_qty ()) (queued false)))
    |}]
;;

let%expect_test "rejects" =
  let v = Venue.create () in
  Venue.new_order v ~account:alice (order Buy 1 ~price:"6700.10" ~cl_ord_id:"tick");
  Venue.new_order v ~account:alice (order Buy 1 ~price:"9000" ~cl_ord_id:"limit");
  Venue.new_order
    v
    ~account:alice
    (order Buy 1 ~price:"1" ~security_id:9 ~cl_ord_id:"sec");
  Venue.new_order v ~account:alice (order Buy 1 ~price:"6700" ~tif:Gtd ~cl_ord_id:"gtd");
  Venue.new_order v ~account:alice (order Buy 0 ~price:"6700" ~cl_ord_id:"qty");
  Venue.new_order
    v
    ~account:alice
    (order Buy 2 ~price:"6700" ~min_qty:1 ~cl_ord_id:"minqty-day");
  Venue.new_order
    v
    ~account:alice
    (order Buy 1 ~ord_type:Stop_limit ~price:"6700" ~cl_ord_id:"stop");
  Venue.new_order
    v
    ~account:alice
    (order Buy 1 ~ord_type:Market_with_protection ~cl_ord_id:"mkt");
  show v;
  [%expect
    {|
    001/ALC Reject tick reason 18: price 6700.1 is not a multiple of the tick 0.25
    001/ALC Reject limit reason 3: price 9000 outside the price limits 6231 to 7169
    001/ALC Reject sec reason 1: unknown security id 9
    001/ALC Reject gtd reason 11: good till date is not supported
    001/ALC Reject qty reason 13: ("contract quantity must be positive" (qty 0))
    001/ALC Reject minqty-day reason 11: minimum quantity is only valid on fill and kill orders
    001/ALC Reject stop reason 11: order type not supported by the simulator
    001/ALC Reject mkt reason 99: no opposite side to protect a market order against
    |}]
;;

let%expect_test "fill and kill, fill or kill, minimum quantity" =
  let v = Venue.create () in
  Venue.new_order v ~account:alice (order Buy 1 ~price:"6700" ~tif:Fak ~cl_ord_id:"fak0");
  show v;
  [%expect
    {|
    001/ALC New fak0 Buy 1 @ 6700
    001/ALC Elimination fak0 cum 0 of 1
      book 1001
    |}];
  Venue.new_order v ~account:bob (order Sell 3 ~price:"6700" ~cl_ord_id:"B1");
  ignore (Venue.drain v : Venue.Output.t list);
  Venue.new_order v ~account:alice (order Buy 5 ~price:"6700" ~tif:Fak ~cl_ord_id:"fak");
  show v;
  [%expect
    {|
    001/ALC New fak Buy 5 @ 6700
    002/BOB Trade B1 3 @ 6700 cum 3 leaves 0
    001/ALC Trade fak 3 @ 6700 cum 3 leaves 2 aggressor
      trade 1001: 3 @ 6700 aggressor (Buy) orders 1
    001/ALC Elimination fak cum 3 of 5
      book 1001
    |}];
  Venue.new_order v ~account:bob (order Sell 3 ~price:"6700" ~cl_ord_id:"B2");
  ignore (Venue.drain v : Venue.Output.t list);
  Venue.new_order v ~account:alice (order Buy 4 ~price:"6700" ~tif:Fok ~cl_ord_id:"fok4");
  Venue.new_order
    v
    ~account:alice
    (order Buy 5 ~price:"6700" ~tif:Fak ~min_qty:4 ~cl_ord_id:"min4");
  Venue.new_order v ~account:alice (order Buy 3 ~price:"6700" ~tif:Fok ~cl_ord_id:"fok3");
  show v;
  [%expect
    {|
    001/ALC New fok4 Buy 4 @ 6700
    001/ALC Elimination fok4 cum 0 of 4
    001/ALC New min4 Buy 5 @ 6700
    001/ALC Elimination min4 cum 0 of 5
    001/ALC New fok3 Buy 3 @ 6700
    002/BOB Trade B2 3 @ 6700 cum 3 leaves 0
    001/ALC Trade fok3 3 @ 6700 cum 3 leaves 0 aggressor
      trade 1001: 3 @ 6700 aggressor (Buy) orders 1
      book 1001
    |}]
;;

let%expect_test "iceberg refreshes its display quantity" =
  let v = Venue.create () in
  Venue.new_order
    v
    ~account:alice
    (order Sell 5 ~price:"6701" ~display_qty:2 ~tif:Gtc ~cl_ord_id:"ice");
  show v;
  [%expect {|
    001/ALC New ice Sell 5 @ 6701
      book 1001
    |}];
  let l2 () =
    let l2 =
      Option.value_exn
        (Exchange_core_engine.Exchange.l2
           (Venue.exchange v)
           (Exchange_core.Symbol.of_string "ESZ6"))
    in
    print_s [%sexp (l2.asks : Exchange_core.L2.Level.t list)]
  in
  l2 ();
  Venue.new_order v ~account:bob (order Buy 3 ~price:"6701" ~cl_ord_id:"take3");
  show v;
  l2 ();
  [%expect
    {|
    (((px 6701) (sz 2) (n 1)))
    002/BOB New take3 Buy 3 @ 6701
    001/ALC Trade ice 2 @ 6701 cum 2 leaves 3
    002/BOB Trade take3 2 @ 6701 cum 2 leaves 1 aggressor
      trade 1001: 2 @ 6701 aggressor (Buy) orders 1
    002/BOB Trade take3 1 @ 6701 cum 3 leaves 0 aggressor
    001/ALC Trade ice 1 @ 6701 cum 3 leaves 2
      trade 1001: 1 @ 6701 aggressor (Buy) orders 1
      book 1001
    (((px 6701) (sz 1) (n 1)))
    |}]
;;

let%expect_test "cancel, replace and their rejects" =
  let v = Venue.create () in
  Venue.new_order v ~account:alice (order Buy 2 ~price:"6699" ~cl_ord_id:"A1");
  let id = last_order_id (Venue.drain v) in
  Venue.replace
    v
    ~account:alice
    (replace_request ~order_id:id ~cl_ord_id:"A1r" ~price:"6699.5" ~qty:4);
  Venue.replace
    v
    ~account:bob
    (replace_request ~order_id:id ~cl_ord_id:"B?" ~price:"6699.5" ~qty:4);
  Venue.replace
    v
    ~account:alice
    (replace_request ~order_id:id ~cl_ord_id:"A1x" ~price:"6699.6" ~qty:4);
  Venue.cancel v ~account:alice (cancel_request ~order_id:id ~cl_ord_id:"A1c");
  Venue.cancel v ~account:alice (cancel_request ~order_id:id ~cl_ord_id:"A1c2");
  show v;
  [%expect
    {|
    001/ALC Modify A1r 4 @ 6699.5 leaves 4
      book 1001
    002/BOB ReplaceReject B?: unknown order
    001/ALC ReplaceReject A1x: price 6699.6 is not a multiple of the tick 0.25
    001/ALC Cancel A1c cum 0
      book 1001
    001/ALC CancelReject A1c2: unknown order
    |}]
;;

let%expect_test "market with protection" =
  let v = Venue.create () in
  Venue.new_order v ~account:bob (order Sell 1 ~price:"6700" ~cl_ord_id:"B1");
  Venue.new_order v ~account:bob (order Sell 1 ~price:"6710" ~cl_ord_id:"B2");
  ignore (Venue.drain v : Venue.Output.t list);
  Venue.new_order
    v
    ~account:alice
    (order Buy 3 ~ord_type:Market_with_protection ~cl_ord_id:"M");
  show v;
  [%expect
    {|
    001/ALC New M Buy 3 @ 6706
    002/BOB Trade B1 1 @ 6700 cum 1 leaves 0
    001/ALC Trade M 1 @ 6700 cum 1 leaves 2 aggressor
      trade 1001: 1 @ 6700 aggressor (Buy) orders 1
      book 1001
    |}]
;;

let%expect_test "phases: close expires day orders, pre-open queues, open releases" =
  let v = Venue.create () in
  Venue.new_order v ~account:alice (order Buy 1 ~price:"6690" ~cl_ord_id:"day");
  Venue.new_order v ~account:alice (order Buy 1 ~price:"6680" ~tif:Gtc ~cl_ord_id:"gtc");
  Venue.new_order v ~account:bob (order Sell 1 ~price:"6695" ~cl_ord_id:"trade");
  Venue.new_order v ~account:alice (order Buy 1 ~price:"6695" ~cl_ord_id:"lift");
  ignore (Venue.drain v : Venue.Output.t list);
  Venue.set_phase v ~security_id:1001 Closed;
  Venue.new_order v ~account:alice (order Buy 1 ~price:"6690" ~cl_ord_id:"closed");
  show v;
  [%expect
    {|
      phase 1001 Closed
    001/ALC Elimination day cum 0 of 1
      book 1001
      settlement 1001 6695
    001/ALC Reject closed reason 2: market is closed
    |}];
  Venue.set_phase v ~security_id:1001 Pre_open;
  Venue.new_order v ~account:alice (order Buy 2 ~price:"6696" ~cl_ord_id:"pre-buy");
  Venue.new_order v ~account:bob (order Sell 1 ~price:"6694" ~cl_ord_id:"pre-sell");
  Venue.new_order
    v
    ~account:bob
    (order Sell 1 ~price:"6694" ~tif:Fak ~cl_ord_id:"pre-fak");
  show v;
  [%expect
    {|
      phase 1001 Pre_open
    001/ALC New pre-buy Buy 2 @ 6696
    002/BOB New pre-sell Sell 1 @ 6694
    002/BOB Reject pre-fak reason 11: market, FAK and FOK orders are not accepted in pre-open
    |}];
  Venue.set_phase v ~security_id:1001 Open;
  show v;
  [%expect
    {|
      phase 1001 Open
      book 1001
    001/ALC Trade pre-buy 1 @ 6696 cum 1 leaves 1
    002/BOB Trade pre-sell 1 @ 6696 cum 1 leaves 0 aggressor
      trade 1001: 1 @ 6696 aggressor (Sell) orders 1
      book 1001
    |}]
;;

let%expect_test "cancel on disconnect" =
  let v = Venue.create () in
  Venue.new_order v ~account:alice (order Buy 1 ~price:"6690" ~cl_ord_id:"a");
  Venue.new_order v ~account:alice (order Sell 1 ~price:"6720" ~cl_ord_id:"b");
  ignore (Venue.drain v : Venue.Output.t list);
  Venue.cancel_on_disconnect v ~account:alice;
  show v;
  [%expect
    {|
    001/ALC Cancel a cum 0 reason 100
      book 1001
    001/ALC Cancel b cum 0 reason 100
      book 1001
    |}]
;;
