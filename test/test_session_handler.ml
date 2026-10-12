open! Core
open Cme_sim
module Ilink3 = Cme_sim_wire.Ilink3

let t0 = Time_ns.of_int_ns_since_epoch 1_760_000_000_000_000_000
let at sec = Time_ns.add t0 (Time_ns.Span.of_sec sec)

let print_actions actions =
  List.iter actions ~f:(fun (a : Session_handler.Action.t) ->
    match a with
    | Send m ->
      let seq =
        Option.value_map (Ilink3.Message.seq_num m) ~default:"" ~f:(sprintf " seq %d")
      in
      let detail =
        match m with
        | Negotiation_response r ->
          sprintf " previous %d/%d" r.previous_uuid r.previous_seq_no
        | Establishment_ack r ->
          sprintf " next %d keepalive %d" r.next_seq_no r.keep_alive_interval
        | Sequence s ->
          sprintf
            " next %d%s"
            s.next_seq_no
            (if s.keep_alive_interval_lapsed then " lapsed" else "")
        | Terminate r -> sprintf " %S" r.reason
        | Not_applied r -> sprintf " from %d count %d" r.from_seq_no r.msg_count
        | Retransmission r -> sprintf " from %d count %d" r.from_seq_no r.msg_count
        | Retransmit_reject r -> sprintf " %S" r.reason
        | Negotiation_reject r -> sprintf " %S" r.reason
        | Establishment_reject r -> sprintf " %S" r.reason
        | Execution_report_new r ->
          sprintf
            " %s%s"
            r.common.cl_ord_id
            (if r.common.poss_retrans then " possdup" else "")
        | _ -> ""
      in
      printf "send %s%s%s\n" (Ilink3.Message.name m) seq detail
    | Business { account; message } ->
      printf
        "venue %s %s\n"
        (Exchange_core.Account.to_string account)
        (Ilink3.Message.name message)
    | Disconnect reason -> printf "disconnect: %s\n" reason)
;;

let negotiate ~uuid : Ilink3.Message.t =
  Negotiate
    { hmac_signature = ""
    ; access_key_id = "KEY"
    ; uuid
    ; request_timestamp = 1
    ; session = "ABC"
    ; firm = "001"
    }
;;

let establish ~uuid ~next : Ilink3.Message.t =
  Establish
    { hmac_signature = ""
    ; access_key_id = "KEY"
    ; trading_system_name = "test"
    ; trading_system_version = "1"
    ; trading_system_vendor = "test"
    ; uuid
    ; request_timestamp = 2
    ; next_seq_no = next
    ; session = "ABC"
    ; firm = "001"
    ; keep_alive_interval = 10_000
    }
;;

let nos seq : Ilink3.Message.t =
  New_order_single
    { price = Some (Exchange_core.Price.of_string "6700")
    ; order_qty = 1
    ; security_id = 1001
    ; side = Buy
    ; seq_num = seq
    ; sender_id = "T"
    ; cl_ord_id = sprintf "C%d" seq
    ; party_details_list_req_id = 1
    ; order_request_id = seq
    ; sending_time = 0
    ; stop_px = None
    ; location = "US,IL"
    ; min_qty = None
    ; display_qty = None
    ; ord_type = Limit
    ; time_in_force = Day
    ; manual = false
    }
;;

let ack cl_ord_id : Ilink3.Message.t =
  Execution_report_new
    { common =
        { seq_num = 0
        ; uuid = 0
        ; exec_id = "X"
        ; sender_id = "T"
        ; cl_ord_id
        ; party_details_list_req_id = 1
        ; order_id = 1
        ; transact_time = 0
        ; sending_time = 0
        ; order_request_id = 1
        ; location = "US,IL"
        ; security_id = 1001
        ; side = Buy
        ; manual = false
        ; poss_retrans = false
        }
    ; price = Exchange_core.Price.of_string "6700"
    ; stop_px = None
    ; order_qty = 1
    ; min_qty = None
    ; display_qty = None
    ; ord_type = Limit
    ; time_in_force = Day
    }
;;

let%expect_test "negotiate, establish, sequence checks" =
  let registry = Session_handler.Registry.create () in
  let s = Session_handler.create registry ~now:t0 in
  let on m ~now = print_actions (Session_handler.on_message s ~now m) in
  on (nos 1) ~now:t0;
  [%expect
    {|
    send Terminate "NewOrderSingle before the session was established"
    disconnect: NewOrderSingle before the session was established
    |}];
  let s = Session_handler.create registry ~now:t0 in
  let on m ~now = print_actions (Session_handler.on_message s ~now m) in
  on (establish ~uuid:7 ~next:1) ~now:t0;
  on (negotiate ~uuid:7) ~now:t0;
  on (establish ~uuid:7 ~next:1) ~now:t0;
  on (nos 1) ~now:t0;
  on (nos 4) ~now:t0;
  print_actions (Session_handler.send_business s ~now:t0 (ack "C1"));
  print_actions (Session_handler.send_business s ~now:t0 (ack "C4"));
  on (nos 2) ~now:t0;
  [%expect
    {|
    send EstablishmentReject "establish before negotiate"
    send NegotiationResponse previous 0/0
    send EstablishmentAck next 1 keepalive 10000
    venue 001/ABC NewOrderSingle
    send NotApplied from 2 count 2
    venue 001/ABC NewOrderSingle
    send ExecutionReportNew seq 1 C1
    send ExecutionReportNew seq 2 C4
    send Terminate "sequence number 2 lower than expected 5"
    disconnect: sequence number 2 lower than expected 5
    |}]
;;

let%expect_test "keep alive" =
  let registry = Session_handler.Registry.create () in
  let s = Session_handler.create registry ~now:t0 in
  ignore (Session_handler.on_message s ~now:t0 (negotiate ~uuid:7) : _ list);
  ignore (Session_handler.on_message s ~now:t0 (establish ~uuid:7 ~next:1) : _ list);
  List.iter [ 5.; 10.; 11.; 15.; 20. ] ~f:(fun sec ->
    printf "t+%g: " sec;
    print_actions (Session_handler.on_timer s ~now:(at sec));
    print_endline "");
  [%expect
    {|
    t+5:
    t+10: send Sequence next 1 lapsed

    t+11:
    t+15:
    t+20: send Terminate "keep alive interval lapsed"
    disconnect: keep alive interval lapsed
    |}]
;;

let%expect_test "no establishment within 60 seconds" =
  let s = Session_handler.create (Session_handler.Registry.create ()) ~now:t0 in
  print_actions (Session_handler.on_timer s ~now:(at 59.));
  print_actions (Session_handler.on_timer s ~now:(at 60.));
  [%expect
    {|
    send Terminate "session not established within 60 seconds"
    disconnect: session not established within 60 seconds
    |}]
;;

let%expect_test "retransmission and reconnecting with the same UUID" =
  let registry = Session_handler.Registry.create () in
  let s = Session_handler.create registry ~now:t0 in
  ignore (Session_handler.on_message s ~now:t0 (negotiate ~uuid:7) : _ list);
  ignore (Session_handler.on_message s ~now:t0 (establish ~uuid:7 ~next:1) : _ list);
  ignore (Session_handler.send_business s ~now:t0 (ack "C1") : _ list);
  (* the connection drops; a report is sequenced while the client is away *)
  Session_handler.store_offline
    registry
    ~account:(Exchange_core.Account.of_string "001/ABC")
    (ack "C2");
  let s = Session_handler.create registry ~now:t0 in
  let on m = print_actions (Session_handler.on_message s ~now:t0 m) in
  on (negotiate ~uuid:7);
  on (establish ~uuid:7 ~next:1);
  on
    (Retransmit_request
       { uuid = 7
       ; last_uuid = None
       ; request_timestamp = 3
       ; from_seq_no = 1
       ; msg_count = 2
       });
  on
    (Retransmit_request
       { uuid = 7
       ; last_uuid = None
       ; request_timestamp = 3
       ; from_seq_no = 2
       ; msg_count = 5
       });
  [%expect
    {|
    send NegotiationResponse previous 7/2
    send EstablishmentAck next 3 keepalive 10000
    send Retransmission from 1 count 2
    send ExecutionReportNew seq 1 C1 possdup
    send ExecutionReportNew seq 2 C2 possdup
    send RetransmitReject "requested range was never sent"
    |}];
  (* a new UUID starts over *)
  let s = Session_handler.create registry ~now:t0 in
  print_actions (Session_handler.on_message s ~now:t0 (negotiate ~uuid:8));
  print_actions (Session_handler.on_message s ~now:t0 (establish ~uuid:8 ~next:1));
  [%expect
    {|
    send NegotiationResponse previous 7/2
    send EstablishmentAck next 1 keepalive 10000
    |}]
;;

let%expect_test "terminate" =
  let s = Session_handler.create (Session_handler.Registry.create ()) ~now:t0 in
  ignore (Session_handler.on_message s ~now:t0 (negotiate ~uuid:7) : _ list);
  print_actions
    (Session_handler.on_message
       s
       ~now:t0
       (Terminate { reason = "bye"; uuid = 7; request_timestamp = 1; error_codes = 0 }));
  [%expect
    {|
    send Terminate "client terminated"
    disconnect: client terminated
    |}]
;;
