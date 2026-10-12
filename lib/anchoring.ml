open! Core
open! Async
module Price = Exchange_core.Price
module Market_sim = Exchange_core_engine.Market_sim

let ns_per_day = Time_ns.Span.to_int_ns Time_ns.Span.day

(* Reference prices are synthetic and rounded to the tick afterwards. *)
let of_float f = (Price.of_float [@alert "-lossy"]) f

module Target = struct
  type t =
    | Series of (Time_ns.t * Price.t) array
    | Fixed of
        { price : Price.t
        ; drift_bps_per_day : float
        }
  [@@deriving sexp_of]

  (* Index of the last point at or before [time], the first point before the series. *)
  let last_at points time =
    match
      Array.binary_search
        points
        ~compare:(fun (t, _) time -> Time_ns.compare t time)
        `Last_less_than_or_equal_to
        time
    with
    | Some i -> i
    | None -> 0
  ;;

  let price_at t ~elapsed =
    let px =
      match t with
      | Series [||] -> None
      | Series points ->
        let first, _ = points.(0) in
        Some (snd points.(last_at points (Time_ns.add first elapsed)))
      | Fixed { price; drift_bps_per_day } ->
        let days =
          Float.of_int (Time_ns.Span.to_int_ns elapsed) /. Float.of_int ns_per_day
        in
        let px =
          Price.to_float price *. ((1. +. (drift_bps_per_day /. 10_000.)) **. days)
        in
        Option.some_if (Float.is_finite px && Float.( < ) px 1e12) (of_float px)
    in
    Option.filter px ~f:Price.is_positive
  ;;
end

module Spec = struct
  type source =
    | Series_file of string
    | Fixed of
        { price : Price.t
        ; drift_bps_per_day : float
        }
    | Scaled of
        { reference : Market_data.Instrument.t
        ; factor : float
        }
  [@@deriving sexp_of]

  type t =
    { source : source
    ; product : Products.t
    }
  [@@deriving sexp_of]

  let product_of_string symbol =
    match Products.find_symbol symbol with
    | Some p -> Ok p
    | None -> Or_error.error_s [%message "unknown symbol" (symbol : string)]
  ;;

  let split_last s ~on ~what =
    match String.rsplit2 s ~on with
    | Some (a, b) when not (String.is_empty a || String.is_empty b) -> Ok (a, b)
    | _ -> Or_error.error_s [%message "expected" (what : string) ~got:(s : string)]
  ;;

  let positive_float s ~what =
    match Float.of_string_opt s with
    | Some f when Float.is_finite f && Float.( > ) f 0. -> Ok f
    | _ -> Or_error.error_s [%message "expected a positive number" (what : string) s]
  ;;

  let positive_price s =
    match Price.of_string_opt s with
    | Some p when Price.is_positive p -> Ok p
    | _ -> Or_error.error_s [%message "expected a positive price" s]
  ;;

  let series_of_string s =
    let open Or_error.Let_syntax in
    let%bind file, symbol = split_last s ~on:'=' ~what:"FILE=SYMBOL" in
    let%map product = product_of_string symbol in
    { source = Series_file file; product }
  ;;

  let fixed_of_string s =
    let open Or_error.Let_syntax in
    let%bind spec, symbol = split_last s ~on:'=' ~what:"PRICE[,DRIFT]=SYMBOL" in
    let%bind product = product_of_string symbol in
    let%map price, drift_bps_per_day =
      match String.lsplit2 spec ~on:',' with
      | None -> positive_price spec >>| fun p -> p, 0.
      | Some (price, drift) ->
        let%bind price = positive_price price in
        (match Float.of_string_opt drift with
         | Some d when Float.is_finite d -> Ok (price, d)
         | _ -> Or_error.error_s [%message "expected a drift in bps per day" drift])
    in
    { source = Fixed { price; drift_bps_per_day }; product }
  ;;

  let scaled_of_string s =
    let open Or_error.Let_syntax in
    let%bind reference, rest =
      split_last s ~on:'=' ~what:"VENUE:SYMBOL=SIM_SYMBOL,FACTOR"
    in
    let%bind symbol, factor = split_last rest ~on:',' ~what:"SIM_SYMBOL,FACTOR" in
    let%bind product = product_of_string symbol in
    let%bind factor = positive_float factor ~what:"factor" in
    let%map reference =
      Or_error.try_with (fun () -> Market_data.Instrument.of_string reference)
    in
    { source = Scaled { reference; factor }; product }
  ;;

  let arg_type parse = Command.Arg_type.create (fun s -> ok_exn (parse s))
end

module Norm_range = struct
  type t =
    { dir : string
    ; from : Date.t
    ; to_ : Date.t
    }
  [@@deriving sexp_of]
end

let scaled_points items ~factor =
  let book = Price_anchor.Ref_book.create () in
  let scale px = of_float (Price.to_float px *. factor) in
  Pipe.fold_without_pushback
    items
    ~init:[]
    ~f:(fun acc (item : Price_anchor.Source.Item.t) ->
      let point ts px = (ts, scale px) :: acc in
      match item with
      | Price { recv_ts; price } -> point recv_ts price
      | Gap _ ->
        Price_anchor.Ref_book.clear book;
        acc
      | Event (Trade trade) -> point trade.recv_ts trade.price
      | Event (Bbo bbo) -> point bbo.recv_ts (Market_data.Bbo.mid bbo)
      | Event (L2 update) ->
        Price_anchor.Ref_book.apply_l2 book update;
        (match Price_anchor.Ref_book.mid book with
         | Some mid -> point update.recv_ts mid
         | None -> acc)
      | Event (Funding _) -> acc)
  >>| fun points -> Array.of_list_rev points
;;

let load ?norm ({ source; product } : Spec.t) =
  let open Deferred.Or_error.Let_syntax in
  let%map target =
    match source with
    | Fixed { price; drift_bps_per_day } ->
      return (Target.Fixed { price; drift_bps_per_day })
    | Series_file file ->
      let%bind items = Price_anchor.Source.of_series file in
      let%bind points = Deferred.ok (scaled_points items ~factor:1.) in
      if Array.is_empty points
      then Deferred.Or_error.error_s [%message "empty series" file]
      else return (Target.Series points)
    | Scaled { reference; factor } ->
      (match norm with
       | None ->
         Deferred.Or_error.error_s
           [%message
             "a scaled anchor needs a normalized data range"
               (reference : Market_data.Instrument.t)]
       | Some { Norm_range.dir; from; to_ } ->
         let%bind items = Price_anchor.Source.of_norm ~dir reference ~from ~to_ in
         let%bind points = Deferred.ok (scaled_points items ~factor) in
         if Array.is_empty points
         then
           Deferred.Or_error.error_s
             [%message "no reference prices" (reference : Market_data.Instrument.t)]
         else return (Target.Series points))
  in
  product, target
;;

module Stats = struct
  type t =
    { pulls : int
    ; waits : int
    ; restarts : int
    ; last_target : Price.t option
    }
  [@@deriving sexp_of]
end

type entry =
  { product : Products.t
  ; target_ : Target.t
  ; mutable stats : Stats.t
  ; mutable was_open : bool
  }

type t =
  { half_life : Time_ns.Span.t
  ; start : Time_ns.t
  ; entries : entry Int.Table.t (** by security id *)
  }

let create ?(half_life = Time_ns.Span.of_sec 1.) ~start targets =
  let entries = Int.Table.create () in
  List.iter targets ~f:(fun ((product : Products.t), target_) ->
    Hashtbl.set
      entries
      ~key:product.security_id
      ~data:
        { product
        ; target_
        ; stats = { pulls = 0; waits = 0; restarts = 0; last_target = None }
        ; was_open = true
        });
  { half_life; start; entries }
;;

let is_anchored t (p : Products.t) = Hashtbl.mem t.entries p.security_id

let target_of t entry ~now =
  Target.price_at entry.target_ ~elapsed:(Time_ns.diff now t.start)
  |> Option.map ~f:(fun px ->
    Equities_core.Tick_size.round ~tick:(Products.tick entry.product) `Nearest px)
  |> Option.filter ~f:Price.is_positive
;;

let target t (p : Products.t) ~now =
  Option.bind (Hashtbl.find t.entries p.security_id) ~f:(target_of t ~now)
;;

let step t (p : Products.t) sim ~venue ~now =
  match Hashtbl.find t.entries p.security_id with
  | None -> ()
  | Some entry ->
    let symbol = Products.engine_symbol p in
    (match Venue.phase venue ~security_id:p.security_id with
     | Open ->
       if not entry.was_open
       then (
         Market_sim.set_reference
           sim
           symbol
           (Venue.last_price venue ~security_id:p.security_id);
         entry.stats <- { entry.stats with restarts = entry.stats.restarts + 1 });
       entry.was_open <- true;
       (match target_of t entry ~now with
        | None -> ()
        | Some px ->
          Market_sim.set_reference ~half_life:t.half_life sim symbol px;
          entry.stats
          <- { entry.stats with pulls = entry.stats.pulls + 1; last_target = Some px })
     | Pre_open | Halted | Closed ->
       entry.was_open <- false;
       entry.stats <- { entry.stats with waits = entry.stats.waits + 1 })
;;

let stats t (p : Products.t) =
  Option.map (Hashtbl.find t.entries p.security_id) ~f:(fun e -> e.stats)
;;
