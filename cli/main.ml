open! Core
open! Async
open Cme_sim
module Ilink3 = Cme_sim_wire.Ilink3
module Mdp3 = Cme_sim_wire.Mdp3
module Mbp_book = Cme_sim_wire.Mbp_book
module Price = Exchange_core.Price

let host_flag =
  Command.Param.(
    flag
      "-host"
      (optional_with_default "127.0.0.1" string)
      ~doc:"HOST (default 127.0.0.1)")
;;

let gateway_flag =
  Command.Param.(
    flag
      "-port"
      (optional_with_default 4300 int)
      ~doc:"PORT iLink 3 gateway (default 4300)")
;;

let feed_port_flag =
  Command.Param.(
    flag "-port" (optional_with_default 4301 int) ~doc:"PORT MDP 3.0 feed (default 4301)")
;;

let session_flags =
  let%map_open.Command firm =
    flag "-firm" (optional_with_default "001" string) ~doc:"FIRM firm id (default 001)"
  and session =
    flag
      "-session"
      (optional_with_default "SIM" string)
      ~doc:"ID session id (default SIM)"
  in
  firm, session
;;

let product_of_symbol s =
  match Products.find_symbol s with
  | Some p -> p
  | None ->
    raise_s
      [%message
        "unknown symbol" s ~known:(List.map Products.all ~f:Products.symbol : string list)]
;;

let side_of_string s : Ilink3.Side.t =
  match String.lowercase s with
  | "buy" | "b" -> Buy
  | "sell" | "s" -> Sell
  | _ -> raise_s [%message "side must be buy or sell" s]
;;

let tif_of_string s : Ilink3.Time_in_force.t =
  match String.lowercase s with
  | "day" -> Day
  | "gtc" -> Gtc
  | "fak" | "ioc" -> Fak
  | "fok" -> Fok
  | _ -> raise_s [%message "tif must be day, gtc, fak (ioc) or fok" s]
;;

let ord_type_of_string s : Ilink3.Ord_type.t =
  match String.lowercase s with
  | "limit" -> Limit
  | "market" -> Market_with_protection
  | _ -> raise_s [%message "type must be limit or market" s]
;;

let print_line s = print_endline s

(* Prints exchange messages until [stop] is determined; heartbeats only with [verbose]. *)
let print_messages ?(verbose = false) ?(on_message = fun _ -> ()) client ~stop =
  let r = Ilink_client.messages client in
  let rec loop () =
    match%bind
      choose
        [ choice stop (fun () -> `Stop)
        ; choice (Pipe.values_available r) (fun x -> `Ready x)
        ]
    with
    | `Stop | `Ready `Eof -> return ()
    | `Ready `Ok ->
      (match Pipe.read_now r with
       | `Ok m ->
         on_message m;
         (match m with
          | Sequence _ when not verbose -> ()
          | _ -> print_line ("< " ^ Ilink_client.describe m))
       | `Eof | `Nothing_available -> ());
      loop ()
  in
  loop ()
;;

let connect ~host ~port ~firm ~session =
  match%bind Ilink_client.connect ~firm ~session ~host ~port () with
  | Error e ->
    eprintf "cannot connect: %s\n" (Error.to_string_hum e);
    exit 1
  | Ok c ->
    let ack = Ilink_client.establishment c in
    print_line
      (sprintf
         "< EstablishmentAck uuid=%d next_seq=%d keep_alive=%dms"
         ack.uuid
         ack.next_seq_no
         ack.keep_alive_interval);
    return c
;;

let wait_flag =
  Command.Param.(
    flag
      "-wait"
      (optional_with_default 1. float)
      ~doc:"SECONDS how long to print responses (default 1)")
;;

let run_and_print ~host ~port ~firm ~session ~wait f =
  let%bind c = connect ~host ~port ~firm ~session in
  f c;
  let%bind () = print_messages c ~stop:(Clock_ns.after (Time_ns.Span.of_sec wait)) in
  Ilink_client.close c
;;

let logon_command =
  Command.async
    ~summary:"Negotiate and establish an iLink 3 session, then terminate it"
    (let%map_open.Command host = host_flag
     and port = gateway_flag
     and firm, session = session_flags in
     fun () -> run_and_print ~host ~port ~firm ~session ~wait:0.2 (fun _ -> ()))
;;

let order_params =
  let%map_open.Command symbol = flag "-symbol" (required string) ~doc:"SYMBOL e.g. ESZ6"
  and side = flag "-side" (required string) ~doc:"SIDE buy or sell"
  and qty = flag "-qty" (required int) ~doc:"N contracts"
  and price = flag "-price" (optional string) ~doc:"PRICE limit price"
  and tif =
    flag "-tif" (optional_with_default "day" string) ~doc:"TIF day, gtc, fak or fok"
  and ord_type =
    flag "-type" (optional_with_default "limit" string) ~doc:"TYPE limit or market"
  and display = flag "-display" (optional int) ~doc:"N iceberg display quantity"
  and min_qty = flag "-min-qty" (optional int) ~doc:"N minimum quantity (FAK only)"
  and cl_ord_id = flag "-clordid" (optional string) ~doc:"ID client order id" in
  fun ~seq_num ->
    let product = product_of_symbol symbol in
    Ilink_client.new_order
      ?price:(Option.map price ~f:Price.of_string)
      ~tif:(tif_of_string tif)
      ~ord_type:(ord_type_of_string ord_type)
      ?display_qty:display
      ?min_qty
      ~security_id:product.security_id
      ~side:(side_of_string side)
      ~qty
      ~cl_ord_id:(Option.value cl_ord_id ~default:(sprintf "CLI-%d" seq_num))
      ~seq_num
      ()
;;

let order_command =
  Command.async
    ~summary:"Send a NewOrderSingle and print the execution reports"
    ~readme:(fun () ->
      "The gateway cancels a session's orders when it disconnects (as Globex does), so a \
       resting order lives for -wait seconds unless the server runs with \
       -no-cancel-on-disconnect. Use the session command to work orders interactively.")
    (let%map_open.Command host = host_flag
     and port = gateway_flag
     and firm, session = session_flags
     and wait = wait_flag
     and build = order_params in
     fun () ->
       run_and_print ~host ~port ~firm ~session ~wait (fun c ->
         Ilink_client.send c (fun ~seq_num -> build ~seq_num)))
;;

let cancel_command =
  Command.async
    ~summary:"Send an OrderCancelRequest"
    (let%map_open.Command host = host_flag
     and port = gateway_flag
     and firm, session = session_flags
     and wait = wait_flag
     and order_id = flag "-id" (required int) ~doc:"ORDERID exchange order id"
     and symbol = flag "-symbol" (optional_with_default "ESZ6" string) ~doc:"SYMBOL"
     and side = flag "-side" (optional_with_default "buy" string) ~doc:"SIDE" in
     fun () ->
       let product = product_of_symbol symbol in
       run_and_print ~host ~port ~firm ~session ~wait (fun c ->
         Ilink_client.send
           c
           (Ilink_client.cancel
              ~order_id
              ~security_id:product.security_id
              ~side:(side_of_string side)
              ~cl_ord_id:"CLI-CXL")))
;;

(* {2 Interactive session} *)

module Script = struct
  (* "order -symbol ESZ6 -side buy" -> [("symbol", "ESZ6"); ("side", "buy")] *)
  let args words =
    let rec loop acc = function
      | key :: value :: rest when String.is_prefix key ~prefix:"-" ->
        loop ((String.drop_prefix key 1, value) :: acc) rest
      | [] -> Ok (List.rev acc)
      | w :: _ -> Or_error.error_s [%message "expected -flag value" w]
    in
    loop [] words
  ;;

  type order_info =
    { security_id : int
    ; side : Ilink3.Side.t
    }

  let run c lines =
    let known = Int.Table.create () in
    let last = ref None in
    let stop = Ivar.create () in
    let printer =
      print_messages c ~stop:(Ivar.read stop) ~on_message:(function
        | Execution_report_new r ->
          Hashtbl.set
            known
            ~key:r.common.order_id
            ~data:{ security_id = r.common.security_id; side = r.common.side };
          last := Some r.common.order_id
        | _ -> ())
    in
    let n = ref 0 in
    let order_id args =
      match List.Assoc.find args ~equal:String.equal "id" with
      | None | Some "last" -> Option.value_exn ~message:"no order yet" !last
      | Some id -> Int.of_string id
    in
    let step line =
      let words = String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty) in
      incr n;
      match words with
      | [] -> return ()
      | w :: _ when String.is_prefix w ~prefix:"#" -> return ()
      | "sleep" :: [ s ] -> Clock_ns.after (Time_ns.Span.of_sec (Float.of_string s))
      | cmd :: rest ->
        print_line ("> " ^ line);
        (match
           Or_error.try_with_join (fun () ->
             let%map.Or_error args = args rest in
             let get k = List.Assoc.find args ~equal:String.equal k in
             let get_exn k =
               Option.value_exn ~message:(sprintf "missing -%s" k) (get k)
             in
             match cmd with
             | "order" ->
               let product = product_of_symbol (get_exn "symbol") in
               Ilink_client.send
                 c
                 (Ilink_client.new_order
                    ?price:(Option.map (get "price") ~f:Price.of_string)
                    ~tif:(tif_of_string (Option.value (get "tif") ~default:"day"))
                    ~ord_type:
                      (ord_type_of_string (Option.value (get "type") ~default:"limit"))
                    ?display_qty:(Option.map (get "display") ~f:Int.of_string)
                    ?min_qty:(Option.map (get "min-qty") ~f:Int.of_string)
                    ~security_id:product.security_id
                    ~side:(side_of_string (get_exn "side"))
                    ~qty:(Int.of_string (get_exn "qty"))
                    ~cl_ord_id:(Option.value (get "clordid") ~default:(sprintf "S-%d" !n))
                    ())
             | "cancel" ->
               let order_id = order_id args in
               let info = Hashtbl.find known order_id in
               Ilink_client.send
                 c
                 (Ilink_client.cancel
                    ~order_id
                    ~security_id:
                      (Option.value_map info ~default:1001 ~f:(fun i -> i.security_id))
                    ~side:
                      (Option.value_map info ~default:Ilink3.Side.Buy ~f:(fun i -> i.side))
                    ~cl_ord_id:(sprintf "S-%d" !n))
             | "replace" ->
               let order_id = order_id args in
               let info = Hashtbl.find known order_id in
               Ilink_client.send
                 c
                 (Ilink_client.replace
                    ~order_id
                    ~security_id:
                      (Option.value_map info ~default:1001 ~f:(fun i -> i.security_id))
                    ~side:
                      (Option.value_map info ~default:Ilink3.Side.Buy ~f:(fun i -> i.side))
                    ~price:(Price.of_string (get_exn "price"))
                    ~qty:(Int.of_string (get_exn "qty"))
                    ~cl_ord_id:(sprintf "S-%d" !n)
                    ())
             | other -> raise_s [%message "unknown command" other])
         with
         | Ok () -> ()
         | Error e -> print_line ("! " ^ Error.to_string_hum e));
        return ()
    in
    let%bind () = Deferred.List.iter ~how:`Sequential lines ~f:step in
    let%bind () = Clock_ns.after (Time_ns.Span.of_ms 300.) in
    Ivar.fill_if_empty stop ();
    let%bind () = printer in
    Ilink_client.close c
  ;;
end

let session_command =
  Command.async
    ~summary:"Keep one session open and run commands read from stdin"
    ~readme:(fun () ->
      "One command per line:\n\
      \  order -symbol ESZ6 -side buy -qty 1 -price 6700.25 [-tif day|gtc|fak|fok] \
       [-type limit|market] [-display N] [-min-qty N]\n\
      \  cancel -id ORDERID|last\n\
      \  replace -id ORDERID|last -price P -qty N\n\
      \  sleep SECONDS\n\
       Lines starting with # are ignored. Responses are printed as they arrive.")
    (let%map_open.Command host = host_flag
     and port = gateway_flag
     and firm, session = session_flags in
     fun () ->
       let%bind c = connect ~host ~port ~firm ~session in
       let%bind lines = Reader.contents (Lazy.force Reader.stdin) in
       Script.run c (String.split_lines lines))
;;

(* {2 Feed} *)

let px = Price.to_string

let symbol id =
  match Products.find_security_id id with
  | Some p -> Products.symbol p
  | None -> Int.to_string id
;;

let describe_mdp (m : Mdp3.Message.t) =
  match m with
  | Admin_heartbeat -> "AdminHeartbeat"
  | Channel_reset _ -> "ChannelReset"
  | Instrument_definition_future d ->
    sprintf
      "Definition %s id=%d tick=%s multiplier=%s limits=%s..%s"
      d.symbol
      d.security_id
      (px d.min_price_increment)
      (Option.value_map d.contract_multiplier ~default:"-" ~f:Int.to_string)
      (Option.value_map d.low_limit_price ~default:"-" ~f:px)
      (Option.value_map d.high_limit_price ~default:"-" ~f:px)
  | Security_status s ->
    sprintf
      "SecurityStatus %s %s"
      (Option.value_map s.security_id ~default:s.security_group ~f:symbol)
      (Option.value_map s.trading_status ~default:"-" ~f:(fun s ->
         Sexp.to_string [%sexp (s : Mdp3.Trading_status.t)]))
  | Snapshot_full_refresh s ->
    sprintf
      "Snapshot %s rpt=%d last_seq=%d %s"
      (symbol s.security_id)
      s.rpt_seq
      s.last_msg_seq_num_processed
      (List.map s.entries ~f:(fun e ->
         sprintf
           "%c%s:%s%s"
           e.entry_type
           (Option.value_map e.price_level ~default:"" ~f:(sprintf "L%d"))
           (Option.value_map e.px ~default:"-" ~f:px)
           (Option.value_map e.size ~default:"" ~f:(sprintf "x%d")))
       |> String.concat ~sep:" ")
  | Incremental_book b ->
    "Book "
    ^ (List.map b.entries ~f:(fun e ->
         sprintf
           "%s %s %s L%d %s%s"
           (symbol e.security_id)
           (match e.entry_type with
            | Bid -> "bid"
            | Offer -> "ask"
            | _ -> "?")
           (Sexp.to_string [%sexp (e.update_action : Mdp3.Update_action.t)]
            |> String.lowercase)
           e.price_level
           (Option.value_map e.px ~default:"-" ~f:px)
           (Option.value_map e.size ~default:"" ~f:(sprintf "x%d")))
       |> String.concat ~sep:", ")
  | Trade_summary t ->
    "Trade "
    ^ (List.map t.entries ~f:(fun e ->
         sprintf
           "%s %d @ %s aggressor=%s id=%s"
           (symbol e.security_id)
           e.size
           (px e.px)
           (match e.aggressor_side with
            | Some Buy -> "buy"
            | Some Sell -> "sell"
            | Some No_aggressor | None -> "none")
           (Option.value_map e.md_trade_entry_id ~default:"-" ~f:Int.to_string))
       |> String.concat ~sep:", ")
  | Daily_statistics d ->
    "DailyStatistics "
    ^ (List.map d.entries ~f:(fun e ->
         sprintf
           "%s %s=%s"
           (symbol e.security_id)
           (match e.entry_type with
            | '6' -> "settlement"
            | c -> String.of_char c)
           (Option.value_map e.px ~default:"-" ~f:px))
       |> String.concat ~sep:", ")
;;

(* Calls [f] on every packet for [seconds] (or until [count] packets). *)
let read_feed ~host ~port ~seconds ~count ~f =
  let%bind _, reader, writer =
    Tcp.connect (Tcp.Where_to_connect.of_host_and_port { host; port })
  in
  let deadline = Clock_ns.after (Time_ns.Span.of_sec seconds) in
  let rec loop n =
    if Option.value_map count ~default:false ~f:(fun c -> n >= c)
    then return ()
    else (
      match%bind
        choose
          [ choice deadline (fun () -> `Done)
          ; choice (Frame_io.read reader) (fun r -> `Frame r)
          ]
      with
      | `Done | `Frame (Ok `Eof) -> return ()
      | `Frame (Error e) -> return (eprintf "feed error: %s\n" (Error.to_string_hum e))
      | `Frame (Ok (`Ok frame)) ->
        (match Mdp3.decode_frame frame with
         | Error e -> eprintf "undecodable packet: %s\n" (Error.to_string_hum e)
         | Ok packet -> f packet);
        loop (n + 1))
  in
  let%bind () = loop 0 in
  Writer.close writer
;;

let feed_command =
  Command.async
    ~summary:"Connect to the MDP 3.0 TCP feed and print the decoded messages"
    (let%map_open.Command host = host_flag
     and port = feed_port_flag
     and seconds =
       flag
         "-seconds"
         (optional_with_default 3. float)
         ~doc:"S how long to listen (default 3)"
     and count = flag "-count" (optional int) ~doc:"N stop after N packets" in
     fun () ->
       read_feed ~host ~port ~seconds ~count ~f:(fun packet ->
         List.iter packet.messages ~f:(fun m ->
           print_line (sprintf "%6d %s" packet.msg_seq_num (describe_mdp m)))))
;;

let book_command =
  Command.async
    ~summary:"Build an instrument's book from the feed and print it"
    (let%map_open.Command host = host_flag
     and port = feed_port_flag
     and symbol_name = flag "-symbol" (required string) ~doc:"SYMBOL e.g. ESZ6"
     and seconds =
       flag
         "-seconds"
         (optional_with_default 2. float)
         ~doc:"S how long to listen (default 2)"
     in
     fun () ->
       let product = product_of_symbol symbol_name in
       let book = Mbp_book.create () in
       let last_trade = ref None in
       let%map () =
         read_feed ~host ~port ~seconds ~count:None ~f:(fun packet ->
           List.iter packet.messages ~f:(function
             | Snapshot_full_refresh s when s.security_id = product.security_id ->
               Mbp_book.apply_snapshot book s
             | Incremental_book b ->
               List.iter b.entries ~f:(fun e ->
                 if e.security_id = product.security_id
                 then (
                   match Mbp_book.apply book e with
                   | Ok () -> ()
                   | Error err -> eprintf "%s\n" (Error.to_string_hum err)))
             | Trade_summary t ->
               List.iter t.entries ~f:(fun e ->
                 if e.security_id = product.security_id
                 then last_trade := Some (e.px, e.size))
             | _ -> ()))
       in
       print_line
         (sprintf
            "%s book (%d levels each side)%s"
            (Products.symbol product)
            (Mbp_book.depth book)
            (Option.value_map !last_trade ~default:"" ~f:(fun (p, s) ->
               sprintf ", last trade %d @ %s" s (px p))));
       print_line (sprintf "%10s %12s | %-12s %-10s" "bid qty" "bid" "ask" "ask qty");
       let bids = Mbp_book.bids book in
       let offers = Mbp_book.offers book in
       let rows = Int.max (List.length bids) (List.length offers) in
       for i = 0 to rows - 1 do
         let cell levels f = Option.value_map (List.nth levels i) ~default:"" ~f in
         print_line
           (sprintf
              "%10s %12s | %-12s %-10s"
              (cell bids (fun (l : Mbp_book.Level.t) -> sprintf "%d (%d)" l.size l.orders))
              (cell bids (fun l -> px l.px))
              (cell offers (fun l -> px l.px))
              (cell offers (fun l -> sprintf "%d (%d)" l.size l.orders)))
       done)
;;

let command =
  Command.group
    ~summary:"Tools for the cme-sim simulator"
    [ "logon", logon_command
    ; "order", order_command
    ; "cancel", cancel_command
    ; "session", session_command
    ; "feed", feed_command
    ; "book", book_command
    ]
;;

let () = Command_unix.run command
