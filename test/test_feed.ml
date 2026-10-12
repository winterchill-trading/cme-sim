open! Core
open Cme_sim
open Venue_helpers
module Mdp3 = Cme_sim_wire.Mdp3
module Mbp_book = Cme_sim_wire.Mbp_book

let now = 1_760_000_000_000_000_000

let describe (m : Mdp3.Message.t) =
  match m with
  | Instrument_definition_future d ->
    sprintf
      "definition %s id %d tick %s limits %s..%s"
      d.symbol
      d.security_id
      (Exchange_core.Price.to_string d.min_price_increment)
      (Option.value_map d.low_limit_price ~default:"-" ~f:Exchange_core.Price.to_string)
      (Option.value_map d.high_limit_price ~default:"-" ~f:Exchange_core.Price.to_string)
  | Security_status s ->
    sprintf
      "status %s %s"
      (Option.value_map s.security_id ~default:"group" ~f:Int.to_string)
      (Sexp.to_string [%sexp (s.trading_status : Mdp3.Trading_status.t option)])
  | Snapshot_full_refresh s ->
    sprintf
      "snapshot %d rpt %d last seq %d entries %d"
      s.security_id
      s.rpt_seq
      s.last_msg_seq_num_processed
      (List.length s.entries)
  | Incremental_book b ->
    String.concat
      ~sep:"; "
      (List.map b.entries ~f:(fun e ->
         sprintf
           "%s %s L%d %s x%s rpt %d"
           (Sexp.to_string [%sexp (e.update_action : Mdp3.Update_action.t)])
           (Sexp.to_string [%sexp (e.entry_type : Mdp3.Entry_type.t)])
           e.price_level
           (Option.value_map e.px ~default:"-" ~f:Exchange_core.Price.to_string)
           (Option.value_map e.size ~default:"-" ~f:Int.to_string)
           e.rpt_seq))
    |> sprintf "book %s"
  | Trade_summary t ->
    List.map t.entries ~f:(fun e ->
      sprintf
        "trade %d @ %s aggressor %s id %s"
        e.size
        (Exchange_core.Price.to_string e.px)
        (Sexp.to_string [%sexp (e.aggressor_side : Mdp3.Aggressor_side.t option)])
        (Option.value_map e.md_trade_entry_id ~default:"-" ~f:Int.to_string))
    |> String.concat ~sep:"; "
  | Daily_statistics d ->
    List.map d.entries ~f:(fun e ->
      sprintf
        "stats %c %s"
        e.entry_type
        (Option.value_map e.px ~default:"-" ~f:Exchange_core.Price.to_string))
    |> String.concat ~sep:"; "
  | other -> Mdp3.Message.name other
;;

let%expect_test "subscription snapshot, then incrementals that rebuild the book" =
  let v = Venue.create () in
  let feed = Feed.create v in
  let book = Mbp_book.create () in
  let print_packet (p : Mdp3.Packet.t) =
    List.iter p.messages ~f:(fun m ->
      (match m with
       | Incremental_book b ->
         List.iter b.entries ~f:(fun e ->
           if e.security_id = 1001 then ok_exn (Mbp_book.apply book e))
       | Snapshot_full_refresh s when s.security_id = 1001 ->
         Mbp_book.apply_snapshot book s
       | _ -> ());
      printf "%3d %s\n" p.msg_seq_num (describe m))
  in
  let step () = List.iter (Venue.drain v) ~f:(Feed.on_output feed ~now) in
  let unsubscribe = Feed.subscribe feed ~now print_packet in
  [%expect
    {|
    0 definition ESZ6 id 1001 tick 0.25 limits 6231..7169
    0 definition NQZ6 id 1002 tick 0.25 limits 22785..26215
    0 definition CLZ6 id 1003 tick 0.01 limits 53.13..71.87
    0 definition GCZ6 id 1004 tick 0.1 limits 3600..4400
    0 status 1001 (Ready_to_trade)
    0 status 1002 (Ready_to_trade)
    0 status 1003 (Ready_to_trade)
    0 status 1004 (Ready_to_trade)
    0 snapshot 1001 rpt 0 last seq 0 entries 1
    0 snapshot 1002 rpt 0 last seq 0 entries 1
    0 snapshot 1003 rpt 0 last seq 0 entries 1
    0 snapshot 1004 rpt 0 last seq 0 entries 1
    |}];
  Venue.new_order v ~account:alice (order Buy 2 ~price:"6699.75" ~cl_ord_id:"a");
  Venue.new_order v ~account:alice (order Buy 1 ~price:"6699.50" ~cl_ord_id:"b");
  Venue.new_order v ~account:bob (order Sell 3 ~price:"6700.25" ~cl_ord_id:"c");
  step ();
  Venue.new_order v ~account:bob (order Sell 3 ~price:"6699.50" ~cl_ord_id:"d");
  step ();
  Venue.set_phase v ~security_id:1001 Closed;
  step ();
  Feed.heartbeat_if_idle feed ~now:(now + 1) ~interval:(Time_ns.Span.of_sec 1.);
  Feed.heartbeat_if_idle
    feed
    ~now:(now + 2_000_000_000)
    ~interval:(Time_ns.Span.of_sec 1.);
  [%expect
    {|
    1 book New Bid L1 6699.75 x2 rpt 1; New Bid L2 6699.5 x1 rpt 2; New Offer L1 6700.25 x3 rpt 3
    2 trade 2 @ 6699.75 aggressor (Sell) id 1
    3 trade 1 @ 6699.5 aggressor (Sell) id 2
    4 book Delete Bid L1 6699.75 x- rpt 6; Delete Bid L1 6699.5 x- rpt 7
    5 status 1001 (Close)
    6 book Delete Offer L1 6700.25 x- rpt 8
    7 stats 6 6699.5
    8 AdminHeartbeat
    |}];
  print_s
    [%message
      "client book"
        ~bids:(Mbp_book.bids book : Mbp_book.Level.t list)
        ~offers:(Mbp_book.offers book : Mbp_book.Level.t list)];
  [%expect {| ("client book" (bids ()) (offers ())) |}];
  unsubscribe ();
  Venue.new_order v ~account:alice (order Buy 1 ~price:"6699.75" ~cl_ord_id:"e");
  step ();
  [%expect {| |}]
;;

let%expect_test "a late subscriber's snapshot matches the book" =
  let v = Venue.create () in
  let feed = Feed.create v in
  Venue.new_order v ~account:alice (order Buy 2 ~price:"6699.75" ~cl_ord_id:"a");
  Venue.new_order v ~account:bob (order Sell 1 ~price:"6699.75" ~cl_ord_id:"b");
  List.iter (Venue.drain v) ~f:(Feed.on_output feed ~now);
  let book = Mbp_book.create () in
  let _unsubscribe =
    Feed.subscribe feed ~now (fun p ->
      List.iter p.messages ~f:(function
        | Snapshot_full_refresh s when s.security_id = 1001 ->
          Mbp_book.apply_snapshot book s;
          print_s [%sexp (s : Mdp3.Snapshot_full_refresh.t)]
        | _ -> ()))
  in
  [%expect
    {|
    ((last_msg_seq_num_processed 2) (tot_num_reports 4) (security_id 1001)
     (rpt_seq 2) (transact_time 1760000000000000000)
     (last_update_time 1760000000000000000) (trade_date (20370))
     (trading_status (Ready_to_trade)) (high_limit_price (7169))
     (low_limit_price (6231)) (max_price_variation (6))
     (entries
      (((px (6699.75)) (size (1)) (number_of_orders (1)) (price_level (1))
        (trading_reference_date ()) (open_close_settl_flag ())
        (settl_price_type 0) (entry_type 0))
       ((px (6699.75)) (size (1)) (number_of_orders ()) (price_level ())
        (trading_reference_date ()) (open_close_settl_flag ())
        (settl_price_type 0) (entry_type 2))
       ((px (6700)) (size ()) (number_of_orders ()) (price_level ())
        (trading_reference_date (20370)) (open_close_settl_flag ())
        (settl_price_type 3) (entry_type 6)))))
    |}]
;;

let%expect_test "JSON rendering of the same packets" =
  let v = Venue.create () in
  let feed = Feed.create v in
  Venue.new_order v ~account:alice (order Buy 2 ~price:"6699.75" ~cl_ord_id:"a");
  Venue.new_order v ~account:bob (order Sell 1 ~price:"6699.75" ~cl_ord_id:"b");
  let _unsubscribe =
    Feed.subscribe feed ~now (fun p ->
      match p.messages with
      | [ Instrument_definition_future { security_id = 1001; _ } ]
      | [ Incremental_book _ ]
      | [ Trade_summary _ ] ->
        List.iter (Json_feed.of_packet p) ~f:(fun j ->
          print_endline (Yojson.Safe.to_string j))
      | _ -> ())
  in
  List.iter (Venue.drain v) ~f:(Feed.on_output feed ~now);
  [%expect
    {|
    {"seq":0,"time":1760000000000000000,"type":"definition","symbol":"ESZ6","security_id":1001,"security_group":"ES","exchange":"XCME","maturity":"202612","tick":"0.25","multiplier":50,"tick_value":"12.5","unit_of_measure":"IPNT","reference_price":"6699.75","low_limit":"6231","high_limit":"7169","status":"open"}
    {"seq":1,"time":1760000000000000000,"type":"book","transact_time":1760000000000000000,"entries":[{"symbol":"ESZ6","side":"bid","level":1,"action":"new","price":"6699.75","size":1,"orders":1,"rpt_seq":1}]}
    {"seq":2,"time":1760000000000000000,"type":"trade","transact_time":0,"entries":[{"symbol":"ESZ6","price":"6699.75","size":1,"aggressor":"sell","orders":2,"trade_id":1,"rpt_seq":2}]}
    |}]
;;
