open! Core
open! Async
open Cme_sim
module Ilink3 = Cme_sim_wire.Ilink3
module Mdp3 = Cme_sim_wire.Mdp3
module Price = Exchange_core.Price

let start () =
  Log.Global.set_output [];
  let server = Server.create { Server.Config.default with noise = false } in
  let%map ports = Server.start server ~gateway_port:0 ~feed_port:0 ~http_port:0 in
  server, ports
;;

(* Reads exchange messages until [n] business messages arrived, skipping heartbeats. *)
let rec expect_business client n =
  if n = 0
  then return ()
  else (
    match%bind
      Clock_ns.with_timeout
        (Time_ns.Span.of_sec 5.)
        (Pipe.read (Ilink_client.messages client))
    with
    | `Timeout -> return (print_endline "timeout")
    | `Result `Eof -> return (print_endline "eof")
    | `Result (`Ok (Sequence _)) -> expect_business client n
    | `Result (`Ok m) ->
      print_endline (Ilink_client.describe m);
      expect_business client (n - 1))
;;

let%expect_test "order entry and the feed over TCP" =
  let%bind _server, ports = start () in
  let%bind feed_socket =
    Tcp.connect
      (Tcp.Where_to_connect.of_host_and_port { host = "127.0.0.1"; port = ports.feed })
  in
  let _, feed_reader, feed_writer = feed_socket in
  let%bind seller =
    Ilink_client.connect
      ~firm:"001"
      ~session:"SEL"
      ~uuid:1
      ~host:"127.0.0.1"
      ~port:ports.gateway
      ()
    >>| ok_exn
  in
  let%bind buyer =
    Ilink_client.connect
      ~firm:"002"
      ~session:"BUY"
      ~uuid:2
      ~host:"127.0.0.1"
      ~port:ports.gateway
      ()
    >>| ok_exn
  in
  let ack = Ilink_client.establishment buyer in
  print_s
    [%message
      ""
        ~next_seq_no:(ack.next_seq_no : int)
        ~previous_uuid:(ack.previous_uuid : int)
        ~keep_alive_interval:(ack.keep_alive_interval : int)];
  [%expect {| ((next_seq_no 1) (previous_uuid 0) (keep_alive_interval 30000)) |}];
  Ilink_client.send
    seller
    (Ilink_client.new_order
       ~price:(Price.of_string "6701")
       ~security_id:1001
       ~side:Sell
       ~qty:2
       ~cl_ord_id:"S1"
       ());
  let%bind () = expect_business seller 1 in
  Ilink_client.send
    buyer
    (Ilink_client.new_order
       ~price:(Price.of_string "6701")
       ~tif:Fak
       ~security_id:1001
       ~side:Buy
       ~qty:3
       ~cl_ord_id:"B1"
       ());
  let%bind () = expect_business buyer 3 in
  let%bind () = expect_business seller 1 in
  [%expect
    {|
    ExecutionReportNew #1 order_id=700000000001 clordid=S1 sell ESZ6 2 @ 6701 day
    ExecutionReportNew #1 order_id=700000000002 clordid=B1 buy ESZ6 3 @ 6701 fak
    ExecutionReportTradeOutright #2 order_id=700000000002 clordid=B1 buy ESZ6 2 @ 6701 cum=2 leaves=1 aggressor trade_id=1
    ExecutionReportElimination #3 order_id=700000000002 clordid=B1 cum=2 of 3
    ExecutionReportTradeOutright #2 order_id=700000000001 clordid=S1 sell ESZ6 2 @ 6701 cum=2 leaves=0 trade_id=1
    |}];
  (* the feed: snapshots first, then the incrementals of what just happened *)
  let rec read_feed n =
    if n = 0
    then return ()
    else (
      match%bind Frame_io.read feed_reader with
      | Ok (`Ok frame) ->
        let packet = ok_exn (Mdp3.decode_frame frame) in
        (* heartbeats depend on timing *)
        (match packet.messages with
         | [ Admin_heartbeat ] -> read_feed n
         | messages ->
           List.iter messages ~f:(fun m -> print_endline (Mdp3.Message.name m));
           read_feed (n - 1))
      | _ -> return (print_endline "feed closed"))
  in
  let%bind () = read_feed 15 in
  [%expect
    {|
    MDInstrumentDefinitionFuture
    MDInstrumentDefinitionFuture
    MDInstrumentDefinitionFuture
    MDInstrumentDefinitionFuture
    SecurityStatus
    SecurityStatus
    SecurityStatus
    SecurityStatus
    SnapshotFullRefresh
    SnapshotFullRefresh
    SnapshotFullRefresh
    SnapshotFullRefresh
    MDIncrementalRefreshBook
    MDIncrementalRefreshTradeSummary
    MDIncrementalRefreshBook
    |}];
  let%bind () = Ilink_client.close seller in
  let%bind () = Ilink_client.close buyer in
  let%bind () = Writer.close feed_writer in
  return ()
;;

let%expect_test "cancel on disconnect, then replay after reconnecting" =
  let%bind _server, ports = start () in
  let connect () =
    Ilink_client.connect
      ~firm:"003"
      ~session:"COD"
      ~uuid:9
      ~host:"127.0.0.1"
      ~port:ports.gateway
      ()
    >>| ok_exn
  in
  let%bind c = connect () in
  Ilink_client.send
    c
    (Ilink_client.new_order
       ~price:(Price.of_string "6600")
       ~security_id:1001
       ~side:Buy
       ~qty:1
       ~cl_ord_id:"R1"
       ());
  let%bind () = expect_business c 1 in
  let%bind () = Ilink_client.close c in
  let%bind () = Clock_ns.after (Time_ns.Span.of_ms 100.) in
  let%bind c = connect () in
  print_s [%message "" ~next_seq:((Ilink_client.establishment c).next_seq_no : int)];
  Ilink_client.send_raw
    c
    (Retransmit_request
       { uuid = 9
       ; last_uuid = None
       ; request_timestamp = 0
       ; from_seq_no = 1
       ; msg_count = 2
       });
  let%bind () = expect_business c 3 in
  let%bind () = Ilink_client.close c in
  [%expect
    {|
    ExecutionReportNew #1 order_id=700000000001 clordid=R1 buy ESZ6 1 @ 6600 day
    (next_seq 3)
    Retransmission from=1 count=2
    ExecutionReportNew #1 order_id=700000000001 clordid=R1 buy ESZ6 1 @ 6600 day
    ExecutionReportCancel #2 order_id=700000000001 clordid=R1 cum=0 restatement=100
    |}];
  return ()
;;
