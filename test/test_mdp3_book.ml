open! Core
open Cme_sim_wire
open Mdp3
module Price = Exchange_core.Price

let px = Price.of_string

let book : Incremental_book.t =
  { transact_time = 1_760_000_000_000_000_000
  ; match_event_indicator = Match_event.(last_quote_msg lor end_of_event)
  ; entries =
      [ { px = Some (px "6000.25")
        ; size = Some 12
        ; security_id = 1001
        ; rpt_seq = 7
        ; number_of_orders = Some 3
        ; price_level = 1
        ; update_action = New
        ; entry_type = Bid
        }
      ; { px = Some (px "6000.75")
        ; size = None
        ; security_id = 1001
        ; rpt_seq = 8
        ; number_of_orders = None
        ; price_level = 2
        ; update_action = Delete
        ; entry_type = Offer
        }
      ]
  }
;;

let%expect_test "MDIncrementalRefreshBook entry layout" =
  let b = encode_message (Incremental_book book) in
  print_endline (Sbe.hex b);
  [%expect
    {|
    60 00 0b 00 2e 00 01 00 0d 00 00 00 b0 d4 ac c6
    6c 18 84 00 00 20 00 02 80 12 c5 0a 75 05 00 00
    0c 00 00 00 e9 03 00 00 07 00 00 00 03 00 00 00
    01 00 30 ff ff ff 7f 00 80 77 92 28 75 05 00 00
    ff ff ff 7f e9 03 00 00 08 00 00 00 ff ff ff 7f
    02 02 31 ff ff ff 7f 00 18 00 00 00 00 00 00 00
    |}];
  (* MsgSize, header, 11-byte root, then groupSize at 21 *)
  let e = 21 + 3 in
  print_s
    [%message
      ""
        ~group:(Sbe.Group.read b ~pos:21 : int * int * int)
        ~px:(Sbe.get_i64 b ~pos:e : Int64.t)
        ~size:(Sbe.get_i32 b ~pos:(e + 8) : int)
        ~security_id:(Sbe.get_i32 b ~pos:(e + 12) : int)
        ~rpt_seq:(Sbe.get_u32 b ~pos:(e + 16) : int)
        ~orders:(Sbe.get_i32 b ~pos:(e + 20) : int)
        ~level:(Sbe.get_u8 b ~pos:(e + 24) : int)
        ~action:(Sbe.get_u8 b ~pos:(e + 25) : int)
        ~entry_type:(Sbe.get_char b ~pos:(e + 26) : char)
        ~order_id_group:
          (Sbe.Group.read ~eight_byte:true b ~pos:(e + 64) : int * int * int)];
  [%expect
    {|
    ((group (32 2 24)) (px 6000250000000) (size 12) (security_id 1001)
     (rpt_seq 7) (orders 3) (level 1) (action 0) (entry_type 0)
     (order_id_group (24 0 96)))
    |}]
;;

let trade : Trade_summary.t =
  { transact_time = 1_760_000_000_000_000_000
  ; match_event_indicator = Match_event.(last_trade_msg lor end_of_event)
  ; entries =
      [ { px = px "6000.25"
        ; size = 5
        ; security_id = 1001
        ; rpt_seq = 9
        ; number_of_orders = 3
        ; aggressor_side = Some Buy
        ; update_action = New
        ; md_trade_entry_id = Some 4242
        }
      ]
  ; orders = [ { order_id = 900001; last_qty = 5 }; { order_id = 17; last_qty = 3 } ]
  }
;;

let%expect_test "MDIncrementalRefreshTradeSummary layout" =
  let b = encode_message (Trade_summary trade) in
  let e = 24 in
  print_s
    [%message
      ""
        ~size:(Bigstring.length b : int)
        ~aggressor:(Sbe.get_u8 b ~pos:(e + 24) : int)
        ~trade_entry_id:(Sbe.get_u32 b ~pos:(e + 26) : int)
        ~orders:(Sbe.Group.read ~eight_byte:true b ~pos:(e + 32) : int * int * int)
        ~first_order:(Sbe.get_u64 b ~pos:(e + 40) : int)
        ~first_last_qty:(Sbe.get_i32 b ~pos:(e + 48) : int)];
  [%expect
    {|
    ((size 96) (aggressor 1) (trade_entry_id 4242) (orders (16 2 64))
     (first_order 900001) (first_last_qty 5))
    |}]
;;

let stats : Daily_statistics.t =
  { transact_time = 1
  ; match_event_indicator = Match_event.(last_stats_msg lor end_of_event)
  ; entries =
      [ { px = Some (px "6001.5")
        ; size = None
        ; security_id = 1001
        ; rpt_seq = 10
        ; trading_reference_date = Some 20_373
        ; settl_price_type = 0b11
        ; update_action = New
        ; entry_type = '6'
        }
      ]
  }
;;

let snapshot : Snapshot_full_refresh.t =
  { last_msg_seq_num_processed = 100
  ; tot_num_reports = 4
  ; security_id = 1001
  ; rpt_seq = 10
  ; transact_time = 2
  ; last_update_time = 3
  ; trade_date = Some 20_373
  ; trading_status = Some Ready_to_trade
  ; high_limit_price = Some (px "6420")
  ; low_limit_price = Some (px "5580")
  ; max_price_variation = None
  ; entries =
      [ { px = Some (px "6000.25")
        ; size = Some 12
        ; number_of_orders = Some 3
        ; price_level = Some 1
        ; trading_reference_date = None
        ; open_close_settl_flag = None
        ; settl_price_type = 0
        ; entry_type = '0'
        }
      ; { px = Some (px "6001.5")
        ; size = None
        ; number_of_orders = None
        ; price_level = None
        ; trading_reference_date = Some 20_373
        ; open_close_settl_flag = None
        ; settl_price_type = 0b11
        ; entry_type = '6'
        }
      ]
  }
;;

let%expect_test "statistics and snapshot layouts" =
  let b = encode_message (Daily_statistics stats) in
  print_s
    [%message
      ""
        ~settl_type:(Sbe.get_u8 b ~pos:(24 + 22) : int)
        ~entry_type:(Sbe.get_char b ~pos:(24 + 24) : char)];
  let b = encode_message (Snapshot_full_refresh snapshot) in
  print_s
    [%message
      ""
        ~size:(Bigstring.length b : int)
        ~security_id:(Sbe.get_i32 b ~pos:(10 + 8) : int)
        ~status:(Sbe.get_u8 b ~pos:(10 + 34) : int)
        ~high:(Sbe.get_price b ~pos:(10 + 35) : Price.t)
        ~group:(Sbe.Group.read b ~pos:(10 + 59) : int * int * int)
        ~second_level_null:(Sbe.get_i8 b ~pos:(10 + 62 + 22 + 16) : int)];
  [%expect
    {|
    ((settl_type 3) (entry_type 6))
    ((size 116) (security_id 1001) (status 17) (high 6420) (group (22 2 72))
     (second_level_null 127))
    |}]
;;

let%expect_test "round trips" =
  List.iter
    [ Message.Incremental_book book
    ; Trade_summary trade
    ; Daily_statistics stats
    ; Snapshot_full_refresh snapshot
    ]
    ~f:(fun m ->
      let p : Packet.t = { msg_seq_num = 3; sending_time = 4; messages = [ m ] } in
      printf
        "%-36s %s\n"
        (Message.name m)
        (Sexp.to_string
           [%sexp
             (Or_error.map (decode_frame (encode_frame p)) ~f:(Packet.equal p)
              : bool Or_error.t)]));
  [%expect
    {|
    MDIncrementalRefreshBook             (Ok true)
    MDIncrementalRefreshTradeSummary     (Ok true)
    MDIncrementalRefreshDailyStatistics  (Ok true)
    SnapshotFullRefresh                  (Ok true)
    |}]
;;
