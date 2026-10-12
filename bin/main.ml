open! Core
open! Async
open Cme_sim

let command =
  Command.async
    ~summary:"CME Globex futures simulator (iLink 3 and MDP 3.0 SBE over TCP)"
    ~readme:(fun () ->
      "Lists ESZ6, NQZ6, CLZ6 and GCZ6. Order entry speaks iLink 3 (SOFH + SBE, FIXP \
       session layer) on the gateway port; market data is MDP 3.0 packets, each prefixed \
       with a SOFH, on the feed port; the HTTP port serves the same feed as JSON on the \
       /ws websocket, the instrument list on / and /admin/halt?symbol=S. There is no \
       real data behind the book: with -noise, simulated traders keep it moving; the \
       -anchor-* flags pull a product's noise towards a reference price while it is \
       open.")
    (let%map_open.Command gateway_port =
       flag
         "-gateway-port"
         (optional_with_default 4300 int)
         ~doc:"PORT iLink 3 gateway (default 4300)"
     and feed_port =
       flag
         "-feed-port"
         (optional_with_default 4301 int)
         ~doc:"PORT MDP 3.0 TCP feed (default 4301)"
     and http_port =
       flag
         "-http-port"
         (optional_with_default 4302 int)
         ~doc:"PORT HTTP and JSON websocket (default 4302)"
     and symbols =
       flag
         "-symbols"
         (optional (Arg_type.comma_separated string))
         ~doc:"SYMBOL,... instruments the noise traders trade (default all)"
     and noise = flag "-noise" no_arg ~doc:" run noise traders so the book moves"
     and clock =
       flag
         "-clock"
         (optional_with_default
            Session_clock.Mode.Always_open
            Session_clock.Mode.arg_type)
         ~doc:"MODE real, accelerated or always-open (default always-open)"
     and seed = flag "-seed" (optional_with_default 0 int) ~doc:"N noise trader seed"
     and keep_orders =
       flag
         "-no-cancel-on-disconnect"
         no_arg
         ~doc:" keep a session's orders when its connection drops"
     and series =
       flag
         "-anchor-series"
         (listed (Anchoring.Spec.arg_type Anchoring.Spec.series_of_string))
         ~doc:"FILE=SYMBOL pull SYMBOL towards a series of <time> <price> lines"
     and fixed =
       flag
         "-anchor-fixed"
         (listed (Anchoring.Spec.arg_type Anchoring.Spec.fixed_of_string))
         ~doc:"PRICE[,DRIFT_BPS_PER_DAY]=SYMBOL pull SYMBOL towards a fixed price"
     and scaled =
       flag
         "-anchor-scaled"
         (listed (Anchoring.Spec.arg_type Anchoring.Spec.scaled_of_string))
         ~doc:
           "VENUE:SYMBOL=SIMSYM,FACTOR pull SIMSYM towards a recorded instrument's \
            prices times FACTOR (needs -anchor-norm)"
     and norm_dir =
       flag
         "-anchor-norm"
         (optional string)
         ~doc:"DIR normalized event logs for -anchor-scaled"
     and norm_from =
       flag "-anchor-from" (optional date) ~doc:"DATE first UTC day of -anchor-norm"
     and norm_to =
       flag "-anchor-to" (optional date) ~doc:"DATE last UTC day (default -anchor-from)"
     and half_life_ms =
       flag
         "-anchor-half-life-ms"
         (optional_with_default 1000. float)
         ~doc:"MS mean reversion half-life of anchored prices (default 1000)"
     and log_level =
       flag
         "-log-level"
         (optional_with_default `Info Log.Level.arg)
         ~doc:"LEVEL debug, info, error (default info)"
     in
     fun () ->
       Log.Global.set_output [ Log.Output.stderr () ];
       Log.Global.set_level log_level;
       let norm =
         match norm_dir, norm_from with
         | Some dir, Some from ->
           Some
             { Anchoring.Norm_range.dir; from; to_ = Option.value norm_to ~default:from }
         | Some _, None -> failwith "-anchor-norm needs -anchor-from"
         | None, _ -> None
       in
       if Float.( <= ) half_life_ms 0.
       then failwith "-anchor-half-life-ms must be positive";
       let%bind anchors =
         Deferred.Or_error.List.map
           ~how:`Sequential
           (series @ fixed @ scaled)
           ~f:(Anchoring.load ?norm)
         >>| ok_exn
       in
       List.iter anchors ~f:(fun (p, (reference : Anchoring.Reference.t)) ->
         Log.Global.info_s
           [%message
             "anchor"
               ~symbol:(Products.symbol p : string)
               ~start:
                 (Price_anchor.Path.price_at reference.path ~elapsed:Time_ns.Span.zero
                  : Exchange_core.Price.t)
               ~factor:(reference.price_factor : float)]);
       let config : Server.Config.t =
         { Server.Config.default with
           clock
         ; noise
         ; noise_symbols = Option.value symbols ~default:[]
         ; seed
         ; cancel_on_disconnect = not keep_orders
         ; anchors
         ; anchor_half_life = Time_ns.Span.of_ms half_life_ms
         }
       in
       let server = Server.create config in
       let%bind ports = Server.start server ~gateway_port ~feed_port ~http_port in
       Log.Global.info
         "cme-sim: iLink 3 gateway on %d, MDP 3.0 feed on %d, HTTP/ws on %d (clock %s%s)"
         ports.gateway
         ports.feed
         ports.http
         (Sexp.to_string [%sexp (clock : Session_clock.Mode.t)])
         (if noise then ", noise traders on" else "");
       Deferred.never ())
;;

let () = Command_unix.run command
