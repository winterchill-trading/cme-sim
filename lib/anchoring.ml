open! Core
open! Async
module Price = Exchange_core.Price
module Market_sim = Exchange_core_engine.Market_sim
module Anchor = Price_anchor.Anchor
module Path = Price_anchor.Path
module Source = Price_anchor.Source

module Reference = struct
  type t =
    { path : Path.t
    ; price_factor : float
    }
  [@@deriving sexp_of]
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

let load ?norm ({ source; product } : Spec.t) =
  let open Deferred.Or_error.Let_syntax in
  let%map reference =
    match source with
    | Fixed { price; drift_bps_per_day } ->
      let%map path = Deferred.return (Path.fixed ~drift_bps_per_day price) in
      { Reference.path; price_factor = 1. }
    | Series_file file ->
      let%bind items = Source.of_series file in
      let%map path = Source.to_path items in
      { Reference.path; price_factor = 1. }
    | Scaled { reference; factor } ->
      (match norm with
       | None ->
         Deferred.Or_error.error_s
           [%message
             "a scaled anchor needs a normalized data range"
               (reference : Market_data.Instrument.t)]
       | Some { Norm_range.dir; from; to_ } ->
         let%bind items = Source.of_norm ~dir reference ~from ~to_ in
         let%map path = Source.to_path items in
         { Reference.path; price_factor = factor })
  in
  product, reference
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

type t =
  { venue : Venue.t
  ; anchors : Anchor.t Int.Table.t (** by security id *)
  }

let create ?(half_life = Time_ns.Span.of_sec 1.) ?log venue products =
  let exchange = Venue.exchange venue in
  let anchors = Int.Table.create () in
  List.iter products ~f:(fun ((p : Products.t), (reference : Reference.t), sim) ->
    let security_id = p.security_id in
    let sim_symbol = Products.engine_symbol p in
    Hashtbl.set
      anchors
      ~key:security_id
      ~data:
        (Anchor.create
           ~price_factor:reference.price_factor
           ~levels:0
           ~mark_interval:Time_ns.Span.zero
           ~noise_traders:(Market_sim.noise_traders sim sim_symbol)
           ~half_life
           ~may_engage:(fun () -> Venue.Phase.equal (Venue.phase venue ~security_id) Open)
           ~last_trade:(fun () -> Some (Venue.last_price venue ~security_id))
           ~step_clock:false
           ~path:reference.path
           ?log
           ~market_sim:sim
           ~exchange
           ~reference:(Market_data.Instrument.create Cme (Products.symbol p))
           ~sim_symbol
           ()));
  { venue; anchors }
;;

let find t (p : Products.t) = Hashtbl.find t.anchors p.security_id
let is_anchored t p = Option.is_some (find t p)
let target t p = Option.bind (find t p) ~f:Anchor.reference_price

let step t p =
  Option.iter (find t p) ~f:(fun anchor ->
    Anchor.step anchor ~now:(Exchange_core_engine.Exchange.now (Venue.exchange t.venue)))
;;

let stats t p =
  Option.map (find t p) ~f:(fun anchor ->
    let s = Anchor.stats anchor in
    { Stats.pulls = s.marks_set
    ; waits = s.waits
    ; restarts = s.resumes
    ; last_target = Anchor.last_set anchor
    })
;;
