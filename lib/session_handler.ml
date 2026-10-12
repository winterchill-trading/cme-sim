open! Core
module Account = Exchange_core.Account
module Ilink3 = Cme_sim_wire.Ilink3

let max_retransmit = 2500
let establish_deadline = Time_ns.Span.of_sec 60.

(* Error codes in Terminate and the rejects; the values are the simulator's own. *)
let error_protocol = 1
let error_sequence = 2
let error_keep_alive = 3

module Entry = struct
  type t =
    { mutable uuid : int
    ; mutable next_outbound : int
    ; mutable next_inbound : int
    ; store : Ilink3.Message.t Int.Table.t
    }

  let create () =
    { uuid = 0; next_outbound = 1; next_inbound = 1; store = Int.Table.create () }
  ;;
end

module Registry = struct
  type t = Entry.t Account.Table.t

  let create () = Account.Table.create ()
end

module Action = struct
  type t =
    | Send of Ilink3.Message.t
    | Business of
        { account : Account.t
        ; message : Ilink3.Message.t
        }
    | Disconnect of string
  [@@deriving sexp_of]
end

type state =
  | Connected
  | Negotiated
  | Established
  | Terminated

type t =
  { registry : Registry.t
  ; connected_at : Time_ns.t
  ; mutable state : state
  ; mutable account : Account.t option
  ; mutable entry : Entry.t
  ; mutable previous_uuid : int
  ; mutable keep_alive : Time_ns.Span.t
  ; mutable last_received : Time_ns.t
  ; mutable last_sent : Time_ns.t
  ; mutable warned : bool
  }

let create registry ~now =
  { registry
  ; connected_at = now
  ; state = Connected
  ; account = None
  ; entry = Entry.create ()
  ; previous_uuid = 0
  ; keep_alive = Time_ns.Span.of_sec 30.
  ; last_received = now
  ; last_sent = now
  ; warned = false
  }
;;

let account t = t.account

let is_established t =
  match t.state with
  | Established -> true
  | Connected | Negotiated | Terminated -> false
;;

let ns now = Time_ns.to_int_ns_since_epoch now

let send t ~now m =
  t.last_sent <- now;
  Action.Send m
;;

let terminate t ~now ~reason ~error_codes =
  t.state <- Terminated;
  [ send
      t
      ~now
      (Terminate { reason; uuid = t.entry.uuid; request_timestamp = ns now; error_codes })
  ; Disconnect reason
  ]
;;

let sequence t ~now ~lapsed =
  send
    t
    ~now
    (Sequence
       { uuid = t.entry.uuid
       ; next_seq_no = t.entry.next_outbound
       ; keep_alive_interval_lapsed = lapsed
       })
;;

let entry_for registry account =
  Hashtbl.find_or_add registry account ~default:Entry.create
;;

let sequence_message (entry : Entry.t) m =
  let m = Ilink3.Message.with_sequence m ~seq_num:entry.next_outbound ~uuid:entry.uuid in
  Hashtbl.set entry.store ~key:entry.next_outbound ~data:m;
  entry.next_outbound <- entry.next_outbound + 1;
  m
;;

let send_business t ~now m =
  match t.state with
  | Established -> [ send t ~now (sequence_message t.entry m) ]
  | Connected | Negotiated | Terminated ->
    ignore (sequence_message t.entry m : Ilink3.Message.t);
    []
;;

let store_offline registry ~account m =
  ignore (sequence_message (entry_for registry account) m : Ilink3.Message.t)
;;

let on_negotiate t ~now (n : Ilink3.Negotiate.t) =
  match t.state with
  | Connected when String.is_empty n.session || String.is_empty n.firm ->
    [ send
        t
        ~now
        (Negotiation_reject
           { reason = "session and firm are required"
           ; uuid = n.uuid
           ; request_timestamp = n.request_timestamp
           ; error_codes = error_protocol
           })
    ; Disconnect "negotiation rejected"
    ]
  | Connected ->
    let account = Account.of_string (n.firm ^ "/" ^ n.session) in
    let entry = entry_for t.registry account in
    let previous_uuid = entry.uuid in
    let previous_seq_no = entry.next_outbound - 1 in
    if n.uuid <> entry.uuid
    then (
      (* a new UUID starts both sequences again *)
      entry.uuid <- n.uuid;
      entry.next_outbound <- 1;
      entry.next_inbound <- 1;
      Hashtbl.clear entry.store);
    t.entry <- entry;
    t.previous_uuid <- previous_uuid;
    t.account <- Some account;
    t.state <- Negotiated;
    [ send
        t
        ~now
        (Negotiation_response
           { uuid = n.uuid
           ; request_timestamp = n.request_timestamp
           ; secret_key_secure_id_expiration = None
           ; previous_seq_no
           ; previous_uuid
           })
    ]
  | Negotiated | Established | Terminated ->
    [ send
        t
        ~now
        (Negotiation_reject
           { reason = "already negotiated"
           ; uuid = n.uuid
           ; request_timestamp = n.request_timestamp
           ; error_codes = error_protocol
           })
    ]
;;

let on_establish t ~now (e : Ilink3.Establish.t) =
  match t.state with
  | Negotiated when e.uuid = t.entry.uuid ->
    t.state <- Established;
    t.keep_alive
    <- Time_ns.Span.of_int_ms (Int.clamp_exn e.keep_alive_interval ~min:1000 ~max:60_000);
    t.entry.next_inbound <- e.next_seq_no;
    [ send
        t
        ~now
        (Establishment_ack
           { uuid = e.uuid
           ; request_timestamp = e.request_timestamp
           ; next_seq_no = t.entry.next_outbound
           ; previous_seq_no = t.entry.next_outbound - 1
           ; previous_uuid = t.previous_uuid
           ; keep_alive_interval = Time_ns.Span.to_int_ms t.keep_alive
           ; secret_key_secure_id_expiration = None
           })
    ]
  | Connected | Negotiated | Established | Terminated ->
    let reason =
      match t.state with
      | Connected -> "establish before negotiate"
      | Established -> "already established"
      | Negotiated | Terminated -> "UUID does not match the negotiation"
    in
    [ send
        t
        ~now
        (Establishment_reject
           { reason
           ; uuid = e.uuid
           ; request_timestamp = e.request_timestamp
           ; next_seq_no = t.entry.next_outbound
           ; error_codes = error_protocol
           })
    ]
;;

(* Customer sequence check for a message carrying [seq]. *)
let check_inbound t ~now seq ~process =
  let expected = t.entry.next_inbound in
  if seq < expected
  then
    terminate
      t
      ~now
      ~reason:(sprintf "sequence number %d lower than expected %d" seq expected)
      ~error_codes:error_sequence
  else (
    let gap =
      if seq > expected
      then
        [ send
            t
            ~now
            (Not_applied
               { uuid = t.entry.uuid; from_seq_no = expected; msg_count = seq - expected })
        ]
      else []
    in
    t.entry.next_inbound <- seq + 1;
    gap @ process ())
;;

let on_retransmit_request t ~now (r : Ilink3.Retransmit_request.t) =
  let reject reason =
    [ send
        t
        ~now
        (Retransmit_reject
           { reason
           ; uuid = t.entry.uuid
           ; last_uuid = r.last_uuid
           ; request_timestamp = r.request_timestamp
           ; error_codes = error_protocol
           })
    ]
  in
  let last = r.from_seq_no + r.msg_count - 1 in
  if Option.value_map r.last_uuid ~default:false ~f:(fun u -> u <> t.entry.uuid)
     || r.uuid <> t.entry.uuid
  then reject "unknown UUID"
  else if r.msg_count < 1 || r.msg_count > max_retransmit
  then reject (sprintf "message count must be 1 to %d" max_retransmit)
  else if r.from_seq_no < 1 || last >= t.entry.next_outbound
  then reject "requested range was never sent"
  else
    send
      t
      ~now
      (Retransmission
         { uuid = t.entry.uuid
         ; last_uuid = r.last_uuid
         ; request_timestamp = r.request_timestamp
         ; from_seq_no = r.from_seq_no
         ; msg_count = r.msg_count
         })
    :: List.filter_map
         (List.range r.from_seq_no (last + 1))
         ~f:(fun seq ->
           Hashtbl.find t.entry.store seq
           |> Option.map ~f:(fun m -> send t ~now (Ilink3.Message.as_retransmission m)))
;;

let on_message t ~now (m : Ilink3.Message.t) =
  t.last_received <- now;
  t.warned <- false;
  match t.state, m with
  | Terminated, _ -> []
  | _, Negotiate n -> on_negotiate t ~now n
  | _, Establish e -> on_establish t ~now e
  | _, Terminate _ ->
    t.state <- Terminated;
    [ send
        t
        ~now
        (Terminate
           { reason = "client terminated"
           ; uuid = t.entry.uuid
           ; request_timestamp = ns now
           ; error_codes = 0
           })
    ; Disconnect "client terminated"
    ]
  | Established, Sequence s ->
    check_inbound t ~now s.next_seq_no ~process:(fun () ->
      (* Sequence announces the next number; it does not consume one *)
      t.entry.next_inbound <- s.next_seq_no;
      [])
  | Established, Retransmit_request r -> on_retransmit_request t ~now r
  | ( Established
    , (New_order_single _ | Order_cancel_request _ | Order_cancel_replace_request _) ) ->
    let seq = Option.value_exn (Ilink3.Message.seq_num m) in
    check_inbound t ~now seq ~process:(fun () ->
      [ Business { account = Option.value_exn t.account; message = m } ])
  | (Connected | Negotiated), _ ->
    terminate
      t
      ~now
      ~reason:(sprintf "%s before the session was established" (Ilink3.Message.name m))
      ~error_codes:error_protocol
  | Established, _ ->
    terminate
      t
      ~now
      ~reason:(sprintf "unexpected %s from the client" (Ilink3.Message.name m))
      ~error_codes:error_protocol
;;

let on_timer t ~now =
  match t.state with
  | Terminated -> []
  | Connected | Negotiated ->
    if Time_ns.Span.( >= ) (Time_ns.diff now t.connected_at) establish_deadline
    then
      terminate
        t
        ~now
        ~reason:"session not established within 60 seconds"
        ~error_codes:error_protocol
    else []
  | Established ->
    let silent_in = Time_ns.diff now t.last_received in
    if Time_ns.Span.( >= ) silent_in (Time_ns.Span.scale t.keep_alive 2.)
    then
      terminate t ~now ~reason:"keep alive interval lapsed" ~error_codes:error_keep_alive
    else if Time_ns.Span.( >= ) silent_in t.keep_alive && not t.warned
    then (
      t.warned <- true;
      [ sequence t ~now ~lapsed:true ])
    else if Time_ns.Span.( >= ) (Time_ns.diff now t.last_sent) t.keep_alive
    then [ sequence t ~now ~lapsed:false ]
    else []
;;
