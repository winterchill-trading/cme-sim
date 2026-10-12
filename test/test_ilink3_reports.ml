open! Core
open Cme_sim_wire
open Ilink3
module Price = Exchange_core.Price

let common : Exec_common.t =
  { seq_num = 5
  ; uuid = 42
  ; exec_id = "X-1"
  ; sender_id = "TRADER1"
  ; cl_ord_id = "ORD-1"
  ; party_details_list_req_id = 1
  ; order_id = 900001
  ; transact_time = 1_760_000_000_000_000_001
  ; sending_time = 1_760_000_000_000_000_002
  ; order_request_id = 11
  ; location = "US,IL"
  ; security_id = 1001
  ; side = Sell
  ; manual = false
  ; poss_retrans = false
  }
;;

let px = Price.of_string

let new_report =
  Message.Execution_report_new
    { common
    ; price = px "6000.25"
    ; stop_px = None
    ; order_qty = 3
    ; min_qty = None
    ; display_qty = None
    ; ord_type = Limit
    ; time_in_force = Day
    }
;;

let trade =
  Message.Execution_report_trade_outright
    { common
    ; last_px = px "6000.25"
    ; price = px "6000"
    ; sec_exec_id = 77
    ; order_qty = 3
    ; last_qty = 2
    ; cum_qty = 2
    ; md_trade_entry_id = 123
    ; side_trade_id = 1
    ; leaves_qty = 1
    ; trade_date = Some 20_373
    ; ord_status = Partially_filled
    ; ord_type = Limit
    ; time_in_force = Gtc
    ; aggressor = true
    ; fills =
        [ { fill_px = px "6000.25"
          ; fill_qty = 2
          ; fill_exec_id = "1"
          ; fill_yield_type = 0
          }
        ]
    }
;;

let cancel =
  Message.Execution_report_cancel
    { common
    ; price = px "6000"
    ; stop_px = None
    ; order_qty = 3
    ; cum_qty = 2
    ; min_qty = None
    ; display_qty = Some 1
    ; ord_type = Limit
    ; time_in_force = Gtc
    ; exec_restatement_reason = Some 100
    }
;;

let all =
  [ new_report
  ; trade
  ; cancel
  ; Execution_report_reject
      { common
      ; text = "price outside the daily limits"
      ; price = Some (px "9999")
      ; stop_px = None
      ; order_qty = 1
      ; min_qty = None
      ; display_qty = None
      ; ord_rej_reason = 1003
      ; ord_type = Limit
      ; time_in_force = Day
      }
  ; Execution_report_elimination
      { common
      ; price = px "6000"
      ; cum_qty = 1
      ; order_qty = 3
      ; min_qty = Some 1
      ; display_qty = None
      ; ord_type = Limit
      ; time_in_force = Fak
      }
  ; Execution_report_modify
      { common
      ; price = px "5999.75"
      ; stop_px = None
      ; order_qty = 4
      ; cum_qty = 1
      ; leaves_qty = 3
      ; min_qty = None
      ; display_qty = None
      ; ord_type = Limit
      ; time_in_force = Day
      }
  ; Order_cancel_reject
      { seq_num = 9
      ; uuid = 42
      ; text = "unknown order"
      ; exec_id = "X-9"
      ; sender_id = "TRADER1"
      ; cl_ord_id = "ORD-9"
      ; party_details_list_req_id = 1
      ; order_id = 0
      ; transact_time = 1
      ; sending_time = 2
      ; order_request_id = 3
      ; location = "US,IL"
      ; cxl_rej_reason = 1003
      ; manual = false
      ; poss_retrans = false
      }
  ; Order_cancel_replace_reject
      { seq_num = 10
      ; uuid = 42
      ; text = "unknown order"
      ; exec_id = "X-10"
      ; sender_id = "TRADER1"
      ; cl_ord_id = "ORD-10"
      ; party_details_list_req_id = 1
      ; order_id = 0
      ; transact_time = 1
      ; sending_time = 2
      ; order_request_id = 3
      ; location = "US,IL"
      ; cxl_rej_reason = 1003
      ; manual = false
      ; poss_retrans = false
      }
  ; Business_reject
      { seq_num = 11
      ; uuid = 42
      ; text = "unknown security"
      ; sender_id = "TRADER1"
      ; party_details_list_req_id = None
      ; sending_time = 2
      ; business_reject_ref_id = Some 3
      ; location = "US,IL"
      ; ref_seq_num = Some 7
      ; ref_tag_id = Some 48
      ; business_reject_reason = 5
      ; ref_msg_type = "D"
      ; poss_retrans = false
      }
  ]
;;

let%expect_test "responses round trip" =
  List.iter all ~f:(fun m ->
    let b = encode m in
    let header = Sbe.Header.read b ~pos:4 in
    let ok = Or_error.map (decode b) ~f:(Message.equal m) in
    printf
      "%-30s %3d  block %3d  size %3d  %s\n"
      (Message.name m)
      header.template_id
      header.block_length
      (Bigstring.length b)
      (Sexp.to_string [%sexp (ok : bool Or_error.t)]));
  [%expect
    {|
    ExecutionReportNew             522  block 209  size 221  (Ok true)
    ExecutionReportTradeOutright   525  block 235  size 268  (Ok true)
    ExecutionReportCancel          534  block 214  size 226  (Ok true)
    ExecutionReportReject          523  block 467  size 479  (Ok true)
    ExecutionReportElimination     524  block 202  size 214  (Ok true)
    ExecutionReportModify          531  block 217  size 229  (Ok true)
    OrderCancelReject              535  block 409  size 421  (Ok true)
    OrderCancelReplaceReject       536  block 409  size 421  (Ok true)
    BusinessReject                 521  block 330  size 342  (Ok true)
    |}]
;;

let at off = 12 + off

let%expect_test "ExecutionReportNew offsets from the spec summary" =
  let b = encode new_report in
  print_s
    [%message
      ""
        ~exec_id:(Sbe.get_string b ~pos:(at 12) ~len:40 : string)
        ~cl_ord_id:(Sbe.get_string b ~pos:(at 72) ~len:20 : string)
        ~order_id:(Sbe.get_u64 b ~pos:(at 100) : int)
        ~price:(Sbe.get_price b ~pos:(at 108) : Price.t)
        ~security_id:(Sbe.get_i32 b ~pos:(at 169) : int)
        ~order_qty:(Sbe.get_u32 b ~pos:(at 173) : int)
        ~ord_type:(Sbe.get_char b ~pos:(at 189) : char)
        ~side:(Sbe.get_u8 b ~pos:(at 190) : int)
        ~tif:(Sbe.get_u8 b ~pos:(at 191) : int)];
  [%expect
    {|
    ((exec_id X-1) (cl_ord_id ORD-1) (order_id 900001) (price 6000.25)
     (security_id 1001) (order_qty 3) (ord_type 2) (side 2) (tif 0))
    |}]
;;

let%expect_test "ExecutionReportTradeOutright offsets and groups" =
  let b = encode trade in
  print_s
    [%message
      ""
        ~last_px:(Sbe.get_price b ~pos:(at 100) : Price.t)
        ~order_id:(Sbe.get_u64 b ~pos:(at 108) : int)
        ~security_id:(Sbe.get_i32 b ~pos:(at 185) : int)
        ~last_qty:(Sbe.get_u32 b ~pos:(at 193) : int)
        ~cum_qty:(Sbe.get_u32 b ~pos:(at 197) : int)
        ~md_trade_entry_id:(Sbe.get_u32 b ~pos:(at 201) : int)
        ~leaves_qty:(Sbe.get_u32 b ~pos:(at 213) : int)
        ~ord_status:(Sbe.get_u8 b ~pos:(at 221) : int)
        ~side:(Sbe.get_u8 b ~pos:(at 223) : int)
        ~aggressor:(Sbe.get_u8 b ~pos:(at 227) : int)
        ~no_fills:(Sbe.Group.read b ~pos:(at 235) : int * int * int)
        ~no_order_events:(Sbe.Group.read b ~pos:(at 235 + 3 + 15) : int * int * int)];
  [%expect
    {|
    ((last_px 6000.25) (order_id 900001) (security_id 1001) (last_qty 2)
     (cum_qty 2) (md_trade_entry_id 123) (leaves_qty 1) (ord_status 1) (side 2)
     (aggressor 1) (no_fills (15 1 250)) (no_order_events (23 0 268)))
    |}]
;;

let%expect_test "ExecutionReportCancel offsets" =
  let b = encode cancel in
  print_s
    [%message
      ""
        ~cum_qty:(Sbe.get_u32 b ~pos:(at 177) : int)
        ~exec_restatement_reason:(Sbe.get_u8 b ~pos:(at 199) : int)];
  [%expect {| ((cum_qty 2) (exec_restatement_reason 100)) |}]
;;

let%expect_test "sequencing and retransmission flags" =
  let m = Message.with_sequence trade ~seq_num:99 ~uuid:7 in
  let r = Message.as_retransmission m in
  print_s
    [%message
      ""
        ~seq:(Message.seq_num m : int option)
        ~poss_retrans_byte:(Sbe.get_u8 (encode r) ~pos:(at 226) : int)
        ~uuid:(Sbe.get_u64 (encode r) ~pos:(at 4) : int)];
  [%expect {| ((seq (99)) (poss_retrans_byte 1) (uuid 7)) |}]
;;
