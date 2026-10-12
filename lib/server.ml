open! Core
open! Async
module Account = Exchange_core.Account
module Exchange = Exchange_core_engine.Exchange
module Market_sim = Exchange_core_engine.Market_sim
module Ilink3 = Cme_sim_wire.Ilink3
module Mdp3 = Cme_sim_wire.Mdp3

module Config = struct
  type t =
    { clock : Session_clock.Mode.t
    ; noise : bool
    ; noise_symbols : string list
    ; seed : int
    ; cancel_on_disconnect : bool
    ; feed_heartbeat : Time_ns.Span.t
    ; halt_duration : Time_ns.Span.t
    ; reopen_pre_open : Time_ns.Span.t
    ; anchors : (Products.t * Anchoring.Reference.t) list
    ; anchor_half_life : Time_ns.Span.t
    }
  [@@deriving sexp_of]

  let default =
    { clock = Always_open
    ; noise = true
    ; noise_symbols = []
    ; seed = 0
    ; cancel_on_disconnect = true
    ; feed_heartbeat = Time_ns.Span.of_sec 5.
    ; halt_duration = Time_ns.Span.of_sec 30.
    ; reopen_pre_open = Time_ns.Span.of_sec 5.
    ; anchors = []
    ; anchor_half_life = Time_ns.Span.of_sec 1.
    }
  ;;
end

module Ports = struct
  type t =
    { gateway : int
    ; feed : int
    ; http : int
    }
  [@@deriving sexp_of]
end

type t =
  { config : Config.t
  ; venue : Venue.t
  ; feed : Feed.t
  ; registry : Session_handler.Registry.t
  ; live : (Session_handler.t * Writer.t) Account.Table.t
  ; start : Time_ns.t
  ; sims : (Products.t * Market_sim.t) list
  ; halts : Time_ns.t Int.Table.t (** product halted until *)
  ; anchoring : Anchoring.t
  }

let ns now = Time_ns.to_int_ns_since_epoch now

let create (config : Config.t) =
  let venue = Venue.create () in
  let start = Time_ns.now () in
  (* The anchors' paths start at the venue's time when they are created. *)
  Venue.set_time venue (ns start);
  let is_anchored (p : Products.t) =
    List.exists config.anchors ~f:(fun ((a : Products.t), _) ->
      a.security_id = p.security_id)
  in
  (* Anchored products always get noise traders: they are the book's only liquidity. *)
  let noise_products =
    let noise =
      if not config.noise
      then []
      else (
        match config.noise_symbols with
        | [] -> Products.all
        | symbols -> List.filter_map symbols ~f:Products.find_symbol)
    in
    List.filter Products.all ~f:(fun p ->
      is_anchored p || List.exists noise ~f:(fun n -> n.security_id = p.security_id))
  in
  let sims =
    List.mapi noise_products ~f:(fun i p ->
      let symbol = Products.engine_symbol p in
      ( p
      , Market_sim.create
          { Market_sim.Config.default with
            seed = config.seed + i
          ; symbols = [ symbol ]
          ; initial_prices = [ symbol, p.reference_price ]
          ; min_notional = 100.
          ; max_notional = 500.
          }
          (Venue.exchange venue) ))
  in
  let anchoring =
    Anchoring.create
      ~half_life:config.anchor_half_life
      ~log:(fun s -> Log.Global.info_s s)
      venue
      (List.filter_map config.anchors ~f:(fun ((p : Products.t), reference) ->
         List.find_map sims ~f:(fun ((q : Products.t), sim) ->
           Option.some_if (q.security_id = p.security_id) (p, reference, sim))))
  in
  { config
  ; venue
  ; feed = Feed.create venue
  ; registry = Session_handler.Registry.create ()
  ; live = Account.Table.create ()
  ; start
  ; sims
  ; halts = Int.Table.create ()
  ; anchoring
  }
;;

let venue t = t.venue
let feed t = t.feed
let anchoring t = t.anchoring

let send writer (m : Ilink3.Message.t) =
  if not (Writer.is_closed writer) then Frame_io.write writer (Ilink3.encode m)
;;

(* Routes venue outputs: reports to their session (or its offline store), the rest to the
   feed. *)
let pump t =
  let now = Time_ns.now () in
  List.iter (Venue.drain t.venue) ~f:(fun (o : Venue.Output.t) ->
    match o with
    | Report { account; message } ->
      (match Hashtbl.find t.live account with
       | Some (session, writer) ->
         List.iter (Session_handler.send_business session ~now message) ~f:(function
           | Send m -> send writer m
           | Business _ | Disconnect _ -> ())
       | None -> Session_handler.store_offline t.registry ~account message)
    | Trade _ | Book_changed _ | Phase_changed _ | Settlement _ ->
      Feed.on_output t.feed ~now:(ns now) o)
;;

let with_venue t f =
  Venue.set_time t.venue (ns (Time_ns.now ()));
  f t.venue;
  pump t
;;

let halt t ~security_id ~duration =
  Hashtbl.set t.halts ~key:security_id ~data:(Time_ns.add (Time_ns.now ()) duration)
;;

let desired_phase t (p : Products.t) ~now : Venue.Phase.t =
  let scheduled = Session_clock.phase t.config.clock ~start:t.start ~now in
  match Hashtbl.find t.halts p.security_id with
  | None -> scheduled
  | Some until ->
    if Time_ns.( < ) now until
    then Halted
    else if Time_ns.( < ) now (Time_ns.add until t.config.reopen_pre_open)
    then Pre_open
    else (
      Hashtbl.remove t.halts p.security_id;
      scheduled)
;;

(* Noise traders do not keep orders over a close: the book starts empty at the open, apart
   from customer GTC orders. *)
let cancel_noise_orders t (p : Products.t) =
  List.iter t.sims ~f:(fun (_, sim) ->
    List.iter (Market_sim.traders sim) ~f:(fun account ->
      List.iter
        (Exchange.open_orders (Venue.exchange t.venue) account)
        ~f:(fun o ->
          if Exchange_core.Symbol.equal o.symbol (Products.engine_symbol p)
          then
            ignore
              (Exchange.cancel
                 (Venue.exchange t.venue)
                 account
                 ~symbol:o.symbol
                 o.order_id
               : Exchange_core.Cancel_status.t))))
;;

let check_price_limits t =
  List.iter Products.all ~f:(fun p ->
    let security_id = p.security_id in
    match Venue.phase t.venue ~security_id with
    | Open ->
      let low, high = Venue.price_limits t.venue ~security_id in
      let last = Venue.last_price t.venue ~security_id in
      if Exchange_core.Price.(last <= low || last >= high)
      then (
        Log.Global.info "%s: price limit reached, halting" (Products.symbol p);
        halt t ~security_id ~duration:t.config.halt_duration)
    | Pre_open | Halted | Closed -> ())
;;

let tick_clock t =
  let now = Time_ns.now () in
  with_venue t (fun venue ->
    check_price_limits t;
    List.iter Products.all ~f:(fun p ->
      let desired = desired_phase t p ~now in
      if not (Venue.Phase.equal desired (Venue.phase venue ~security_id:p.security_id))
      then (
        Log.Global.info
          "%s: %s"
          (Products.symbol p)
          (Sexp.to_string [%sexp (desired : Venue.Phase.t)]);
        (match desired with
         | Closed | Halted -> cancel_noise_orders t p
         | Open | Pre_open -> ());
        Venue.set_phase venue ~security_id:p.security_id desired));
    Venue.sync venue);
  Feed.heartbeat_if_idle t.feed ~now:(ns now) ~interval:t.config.feed_heartbeat
;;

let tick_noise t =
  with_venue t (fun venue ->
    List.iter t.sims ~f:(fun (p, sim) ->
      Anchoring.step t.anchoring p;
      match Venue.phase venue ~security_id:p.security_id with
      | Open -> Market_sim.tick sim
      | Pre_open | Halted | Closed -> ());
    Venue.sync venue)
;;

(* {2 iLink 3 gateway} *)

let business t ~account (m : Ilink3.Message.t) =
  with_venue t (fun venue ->
    match m with
    | New_order_single r -> Venue.new_order venue ~account r
    | Order_cancel_request r -> Venue.cancel venue ~account r
    | Order_cancel_replace_request r -> Venue.replace venue ~account r
    | _ -> ())
;;

let handle_gateway t addr reader writer =
  Writer.set_raise_when_consumer_leaves writer false;
  let session = Session_handler.create t.registry ~now:(Time_ns.now ()) in
  let closed = Ivar.create () in
  let perform actions =
    List.iter actions ~f:(fun (a : Session_handler.Action.t) ->
      match a with
      | Send m -> send writer m
      | Business { account; message } -> business t ~account message
      | Disconnect reason ->
        Log.Global.info "gateway %s: %s" (Socket.Address.Inet.to_string addr) reason;
        Ivar.fill_if_empty closed ())
  in
  let register () =
    match Session_handler.account session with
    | Some account when Session_handler.is_established session ->
      if not (Hashtbl.mem t.live account)
      then (
        Log.Global.info "gateway: %s established" (Account.to_string account);
        Hashtbl.set t.live ~key:account ~data:(session, writer))
    | _ -> ()
  in
  Clock_ns.every ~stop:(Ivar.read closed) (Time_ns.Span.of_sec 1.) (fun () ->
    perform (Session_handler.on_timer session ~now:(Time_ns.now ())));
  let rec loop () =
    if Ivar.is_full closed
    then return ()
    else (
      match%bind Frame_io.read reader with
      | Ok `Eof -> return ()
      | Error e ->
        Log.Global.info_s [%message "gateway: bad frame" (e : Error.t)];
        return ()
      | Ok (`Ok frame) ->
        (match Ilink3.decode frame with
         | Error e ->
           Log.Global.info_s [%message "gateway: undecodable message" (e : Error.t)]
         | Ok m ->
           perform (Session_handler.on_message session ~now:(Time_ns.now ()) m);
           register ());
        loop ())
  in
  let%bind () = Deferred.any [ loop (); Ivar.read closed ] in
  Ivar.fill_if_empty closed ();
  Option.iter (Session_handler.account session) ~f:(fun account ->
    (match Hashtbl.find t.live account with
     | Some (s, _) when phys_equal s session -> Hashtbl.remove t.live account
     | _ -> ());
    if t.config.cancel_on_disconnect && not (Hashtbl.mem t.live account)
    then with_venue t (fun venue -> Venue.cancel_on_disconnect venue ~account));
  Writer.flushed writer
;;

(* {2 MDP feed over TCP} *)

let handle_feed t _addr reader writer =
  Writer.set_raise_when_consumer_leaves writer false;
  let unsubscribe =
    Feed.subscribe
      t.feed
      ~now:(ns (Time_ns.now ()))
      (fun packet ->
        if not (Writer.is_closed writer)
        then Frame_io.write writer (Mdp3.encode_frame packet))
  in
  let%map () = Deferred.any [ Reader.drain reader; Writer.consumer_left writer ] in
  unsubscribe ()
;;

(* {2 HTTP and the JSON websocket} *)

let json_headers = Cohttp.Header.of_list [ "content-type", "application/json" ]

let respond ?(status = `OK) json =
  let%map response =
    Cohttp_async.Server.respond_string
      ~headers:json_headers
      ~status
      (Yojson.Safe.to_string json)
  in
  `Response response
;;

let handle_ws t reader writer =
  let ws = Websocket.create ~opcode:`Text ~role:Server reader writer in
  let incoming, outgoing = Websocket.pipes ws in
  let unsubscribe =
    Feed.subscribe
      t.feed
      ~now:(ns (Time_ns.now ()))
      (fun packet ->
        List.iter (Json_feed.of_packet packet) ~f:(fun json ->
          Pipe.write_without_pushback_if_open outgoing (Yojson.Safe.to_string json)))
  in
  let%map () = Pipe.drain incoming in
  unsubscribe ();
  Pipe.close outgoing
;;

let is_upgrade req =
  match Cohttp.Header.get (Cohttp.Request.headers req) "upgrade" with
  | Some v -> String.Caseless.equal (String.strip v) "websocket"
  | None -> false
;;

let instruments_json t =
  let now = ns (Time_ns.now ()) in
  `List
    (List.concat_map Products.all ~f:(fun p ->
       Json_feed.of_packet
         { msg_seq_num = Feed.msg_seq_num t.feed
         ; sending_time = now
         ; messages = [ Instrument_definition_future (Feed.definition t.feed ~now p) ]
         }))
;;

let anchors_json t =
  let price px = `String (Exchange_core.Price.to_string px) in
  let opt = Option.value_map ~default:`Null ~f:price in
  `List
    (List.filter_map Products.all ~f:(fun p ->
       Option.map (Anchoring.stats t.anchoring p) ~f:(fun (s : Anchoring.Stats.t) ->
         let security_id = p.security_id in
         `Assoc
           [ "symbol", `String (Products.symbol p)
           ; ( "phase"
             , `String
                 (Sexp.to_string
                    [%sexp (Venue.phase t.venue ~security_id : Venue.Phase.t)]) )
           ; "target", opt (Anchoring.target t.anchoring p)
           ; ( "mark"
             , opt (Exchange.mark (Venue.exchange t.venue) (Products.engine_symbol p)) )
           ; "last", price (Venue.last_price t.venue ~security_id)
           ; ( "limits"
             , let low, high = Venue.price_limits t.venue ~security_id in
               `List [ price low; price high ] )
           ; "pulls", `Int s.pulls
           ; "waits", `Int s.waits
           ; "restarts", `Int s.restarts
           ])))
;;

let handle_http t ~body:_ _addr req =
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/ws" when is_upgrade req ->
    (match Cohttp.Header.get (Cohttp.Request.headers req) "sec-websocket-key" with
     | None -> respond ~status:`Bad_request (`Assoc [ "error", `String "missing key" ])
     | Some key ->
       let headers =
         Cohttp.Header.of_list
           [ "upgrade", "websocket"
           ; "connection", "Upgrade"
           ; ( "sec-websocket-accept"
             , Websocket.sec_websocket_accept_header_value ~sec_websocket_key:key )
           ]
       in
       return
         (`Expert
           ( Http.Response.make ~status:`Switching_protocols ~headers ()
           , fun reader writer -> handle_ws t reader writer )))
  | "/" | "/instruments" -> respond (instruments_json t)
  | "/anchors" -> respond (anchors_json t)
  | "/admin/halt" ->
    let symbol =
      Uri.get_query_param uri "symbol" |> Option.bind ~f:Products.find_symbol
    in
    let seconds =
      Uri.get_query_param uri "seconds"
      |> Option.bind ~f:(fun s -> Float.of_string_opt s)
      |> Option.value ~default:(Time_ns.Span.to_sec t.config.halt_duration)
    in
    (match symbol with
     | None -> respond ~status:`Bad_request (`Assoc [ "error", `String "unknown symbol" ])
     | Some p ->
       halt t ~security_id:p.security_id ~duration:(Time_ns.Span.of_sec seconds);
       tick_clock t;
       respond
         (`Assoc [ "halted", `String (Products.symbol p); "seconds", `Float seconds ]))
  | path -> respond ~status:`Not_found (`Assoc [ "error", `String ("no route " ^ path) ])
;;

let start t ~gateway_port ~feed_port ~http_port =
  let%bind gateway =
    Tcp.Server.create
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.of_port gateway_port)
      (handle_gateway t)
  in
  let%bind feed =
    Tcp.Server.create
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.of_port feed_port)
      (handle_feed t)
  in
  let%map http =
    Cohttp_async.Server.create_expert
      ~on_handler_error:
        (`Call (fun _ exn -> Log.Global.error_s [%message "http handler" (exn : exn)]))
      (Tcp.Where_to_listen.of_port http_port)
      (handle_http t)
  in
  tick_clock t;
  Clock_ns.every (Time_ns.Span.of_ms 200.) (fun () -> tick_clock t);
  (match t.sims with
   | [] -> ()
   | (_, sim) :: _ ->
     Clock_ns.every (Market_sim.config sim).tick_interval (fun () -> tick_noise t));
  { Ports.gateway = Tcp.Server.listening_on gateway
  ; feed = Tcp.Server.listening_on feed
  ; http = Cohttp_async.Server.listening_on http
  }
;;
