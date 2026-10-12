open! Core
module Price = Exchange_core.Price
module Cme_product = Equities_core.Tick_size.Cme_product
module Future = Equities_core.Symbol.Future

type t =
  { product : Cme_product.t
  ; future : Future.t
  ; security_id : int
  ; reference_price : Price.t
  ; limit_percent : int
  ; protection : Price.t
  ; unit_of_measure : string
  ; max_trade_vol : int
  }
[@@deriving sexp_of]

let p = Price.of_string

let make product ~security_id ~reference_price ~limit_percent ~protection ~unit_of_measure
  =
  { product
  ; future = { root = product.Cme_product.root; month = Dec; year = 2026 }
  ; security_id
  ; reference_price = p reference_price
  ; limit_percent
  ; protection = p protection
  ; unit_of_measure
  ; max_trade_vol = 2000
  }
;;

let all =
  [ make
      Cme_product.es
      ~security_id:1001
      ~reference_price:"6700"
      ~limit_percent:7
      ~protection:"6"
      ~unit_of_measure:"IPNT"
  ; make
      Cme_product.nq
      ~security_id:1002
      ~reference_price:"24500"
      ~limit_percent:7
      ~protection:"25"
      ~unit_of_measure:"IPNT"
  ; make
      Cme_product.cl
      ~security_id:1003
      ~reference_price:"62.50"
      ~limit_percent:15
      ~protection:"0.50"
      ~unit_of_measure:"BBL"
  ; make
      Cme_product.gc
      ~security_id:1004
      ~reference_price:"4000"
      ~limit_percent:10
      ~protection:"5"
      ~unit_of_measure:"OZTR"
  ]
;;

let symbol t = Future.to_string t.future

let find_symbol s =
  let s = String.uppercase s in
  List.find all ~f:(fun t -> String.equal (symbol t) s)
;;

let find_security_id id = List.find all ~f:(fun t -> t.security_id = id)
let tick t = t.product.tick
let multiplier t = t.product.multiplier
let engine_symbol t = Exchange_core.Symbol.of_string (symbol t)

let instrument t =
  Exchange_core.Instrument.create
    ~fees:Exchange_core.Fee_schedule.zero
    (engine_symbol t)
    ~size_decimals:0
    ~price_tick:(tick t)
    ~max_leverage:50
;;

let price_limits t ~reference =
  let band = Price.div_int (Price.scale reference ~by:t.limit_percent) 100 in
  let round dir px = Equities_core.Tick_size.round ~tick:(tick t) dir px in
  round `Up Price.(reference - band), round `Down Price.(reference + band)
;;

let security_group t = t.product.root
let last_trade_date t = Or_error.ok_exn (Future.last_trade_date t.future)
