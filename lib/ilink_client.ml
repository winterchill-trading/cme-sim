open! Core
open! Async
module Ilink3 = Cme_sim_wire.Ilink3
module Price = Exchange_core.Price

type t =
  { reader : Reader.t
  ; writer : Writer.t
  ; uuid : int
  ; establishment : Ilink3.Establishment_ack.t
  ; messages : Ilink3.Message.t Pipe.Reader.t
  ; mutable next_seq : int
  ; closed : unit Ivar.t
  }

let uuid t = t.uuid
let establishment t = t.establishment
let messages t = t.messages
let now_ns () = Time_ns.to_int_ns_since_epoch (Time_ns.now ())

let send_raw_writer writer m =
  if not (Writer.is_closed writer) then Frame_io.write writer (Ilink3.encode m)
;;

let read_message reader =
  match%map Frame_io.read reader with
  | Error e -> Error e
  | Ok `Eof -> Or_error.error_string "connection closed"
  | Ok (`Ok frame) -> Ilink3.decode frame
;;

let connect
  ?(firm = "001")
  ?(session = "SIM")
  ?uuid
  ?(keep_alive_ms = 30_000)
  ~host
  ~port
  ()
  =
  let uuid = Option.value uuid ~default:(now_ns () / 1000) in
  Deferred.Or_error.try_with_join (fun () ->
    let%bind _socket, reader, writer =
      Tcp.connect (Tcp.Where_to_connect.of_host_and_port { host; port })
    in
    let fail e =
      let%map () = Writer.close writer in
      Error e
    in
    send_raw_writer
      writer
      (Negotiate
         { hmac_signature = String.make 32 '0'
         ; access_key_id = "SIMULATOR"
         ; uuid
         ; request_timestamp = now_ns ()
         ; session
         ; firm
         });
    match%bind read_message reader with
    | Error e -> fail e
    | Ok (Negotiation_response _) ->
      send_raw_writer
        writer
        (Establish
           { hmac_signature = String.make 32 '0'
           ; access_key_id = "SIMULATOR"
           ; trading_system_name = "cme-sim-cli"
           ; trading_system_version = "0.1"
           ; trading_system_vendor = "cme-sim"
           ; uuid
           ; request_timestamp = now_ns ()
           ; next_seq_no = 1
           ; session
           ; firm
           ; keep_alive_interval = keep_alive_ms
           });
      (match%bind read_message reader with
       | Error e -> fail e
       | Ok (Establishment_ack establishment) ->
         let messages_r, messages_w = Pipe.create () in
         let closed = Ivar.create () in
         let t =
           { reader
           ; writer
           ; uuid
           ; establishment
           ; messages = messages_r
           ; next_seq = 1
           ; closed
           }
         in
         Clock_ns.every
           ~stop:(Ivar.read closed)
           (Time_ns.Span.of_int_ms (keep_alive_ms / 2))
           (fun () ->
             send_raw_writer
               writer
               (Sequence
                  { uuid; next_seq_no = t.next_seq; keep_alive_interval_lapsed = false }));
         don't_wait_for
           (let rec loop () =
              match%bind read_message reader with
              | Error _ ->
                Ivar.fill_if_empty closed ();
                Pipe.close messages_w;
                return ()
              | Ok m ->
                let%bind () = Pipe.write_if_open messages_w m in
                loop ()
            in
            loop ());
         return (Ok t)
       | Ok other ->
         fail
           (Error.create_s
              [%message "establishment failed" ~got:(Ilink3.Message.name other : string)]))
    | Ok other ->
      fail
        (Error.create_s
           [%message "negotiation failed" ~got:(Ilink3.Message.name other : string)]))
;;

let send t f =
  let m = f ~seq_num:t.next_seq in
  t.next_seq <- t.next_seq + 1;
  send_raw_writer t.writer m
;;

let send_raw t m = send_raw_writer t.writer m

let close t =
  send_raw_writer
    t.writer
    (Terminate
       { reason = "done"; uuid = t.uuid; request_timestamp = now_ns (); error_codes = 0 });
  Ivar.fill_if_empty t.closed ();
  let%bind () = Writer.flushed t.writer in
  (* give the gateway a moment to answer before the socket goes *)
  let%bind () = Clock_ns.after (Time_ns.Span.of_ms 50.) in
  let%bind () = Writer.close t.writer in
  Reader.close t.reader
;;

let new_order
  ?price
  ?(tif = Ilink3.Time_in_force.Day)
  ?(ord_type = Ilink3.Ord_type.Limit)
  ?display_qty
  ?min_qty
  ~security_id
  ~side
  ~qty
  ~cl_ord_id
  ~seq_num
  ()
  : Ilink3.Message.t
  =
  New_order_single
    { price
    ; order_qty = qty
    ; security_id
    ; side
    ; seq_num
    ; sender_id = "CLI"
    ; cl_ord_id
    ; party_details_list_req_id = 1
    ; order_request_id = seq_num
    ; sending_time = now_ns ()
    ; stop_px = None
    ; location = "US,IL"
    ; min_qty
    ; display_qty
    ; ord_type
    ; time_in_force = tif
    ; manual = true
    }
;;

let cancel ~order_id ~security_id ~side ~cl_ord_id ~seq_num : Ilink3.Message.t =
  Order_cancel_request
    { order_id
    ; party_details_list_req_id = 1
    ; manual = true
    ; seq_num
    ; sender_id = "CLI"
    ; cl_ord_id
    ; order_request_id = seq_num
    ; sending_time = now_ns ()
    ; location = "US,IL"
    ; security_id
    ; side
    }
;;

let replace
  ?(tif = Ilink3.Time_in_force.Day)
  ~order_id
  ~security_id
  ~side
  ~price
  ~qty
  ~cl_ord_id
  ~seq_num
  ()
  : Ilink3.Message.t
  =
  Order_cancel_replace_request
    { price = Some price
    ; order_qty = qty
    ; security_id
    ; side
    ; seq_num
    ; sender_id = "CLI"
    ; cl_ord_id
    ; party_details_list_req_id = 1
    ; order_id
    ; stop_px = None
    ; order_request_id = seq_num
    ; sending_time = now_ns ()
    ; location = "US,IL"
    ; min_qty = None
    ; display_qty = None
    ; ord_type = Limit
    ; time_in_force = tif
    ; manual = true
    ; ofm_override = false
    }
;;

let symbol security_id =
  match Products.find_security_id security_id with
  | Some p -> Products.symbol p
  | None -> Int.to_string security_id
;;

let side (s : Ilink3.Side.t) =
  match s with
  | Buy -> "buy"
  | Sell -> "sell"
;;

let tif t = Sexp.to_string [%sexp (t : Ilink3.Time_in_force.t)] |> String.lowercase
let px = Price.to_string

let describe (m : Ilink3.Message.t) =
  let name = Ilink3.Message.name m in
  let seq = Option.value_map (Ilink3.Message.seq_num m) ~default:"" ~f:(sprintf "#%d ") in
  let body =
    match m with
    | Negotiation_response r -> sprintf "uuid=%d previous_seq=%d" r.uuid r.previous_seq_no
    | Establishment_ack r ->
      sprintf
        "uuid=%d next_seq=%d keep_alive=%dms"
        r.uuid
        r.next_seq_no
        r.keep_alive_interval
    | Sequence r ->
      sprintf
        "next_seq=%d%s"
        r.next_seq_no
        (if r.keep_alive_interval_lapsed then " lapsed" else "")
    | Terminate r -> sprintf "reason=%S" r.reason
    | Not_applied r -> sprintf "from=%d count=%d" r.from_seq_no r.msg_count
    | Retransmission r -> sprintf "from=%d count=%d" r.from_seq_no r.msg_count
    | Execution_report_new r ->
      sprintf
        "order_id=%d clordid=%s %s %s %d @ %s %s"
        r.common.order_id
        r.common.cl_ord_id
        (side r.common.side)
        (symbol r.common.security_id)
        r.order_qty
        (px r.price)
        (tif r.time_in_force)
    | Execution_report_reject r ->
      sprintf "clordid=%s reason=%d %S" r.common.cl_ord_id r.ord_rej_reason r.text
    | Execution_report_trade_outright r ->
      sprintf
        "order_id=%d clordid=%s %s %s %d @ %s cum=%d leaves=%d%s trade_id=%d"
        r.common.order_id
        r.common.cl_ord_id
        (side r.common.side)
        (symbol r.common.security_id)
        r.last_qty
        (px r.last_px)
        r.cum_qty
        r.leaves_qty
        (if r.aggressor then " aggressor" else "")
        r.md_trade_entry_id
    | Execution_report_elimination r ->
      sprintf
        "order_id=%d clordid=%s cum=%d of %d"
        r.common.order_id
        r.common.cl_ord_id
        r.cum_qty
        r.order_qty
    | Execution_report_cancel r ->
      sprintf
        "order_id=%d clordid=%s cum=%d%s"
        r.common.order_id
        r.common.cl_ord_id
        r.cum_qty
        (Option.value_map
           r.exec_restatement_reason
           ~default:""
           ~f:(sprintf " restatement=%d"))
    | Execution_report_modify r ->
      sprintf
        "order_id=%d clordid=%s %d @ %s leaves=%d"
        r.common.order_id
        r.common.cl_ord_id
        r.order_qty
        (px r.price)
        r.leaves_qty
    | Order_cancel_reject r | Order_cancel_replace_reject r ->
      sprintf "order_id=%d clordid=%s %S" r.order_id r.cl_ord_id r.text
    | Business_reject r -> sprintf "%S" r.text
    | _ -> ""
  in
  sprintf "%s %s%s" name seq body
;;
