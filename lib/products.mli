(** The simulated Globex instruments: the December 2026 E-mini S&P 500, E-mini Nasdaq-100,
    WTI crude oil and gold futures.

    Ticks and multipliers come from {!Equities_core.Tick_size.Cme_product}. Security ids,
    reference prices and protection ranges are the simulator's own (CME assigns security
    ids per listing and publishes prices daily). *)

open! Core
module Price := Exchange_core.Price

type t =
  { product : Equities_core.Tick_size.Cme_product.t
  ; future : Equities_core.Symbol.Future.t
  ; security_id : int
  ; reference_price : Price.t (** prior settlement the simulation starts from *)
  ; limit_percent : int (** daily price limit around the reference, in percent *)
  ; protection : Price.t (** market order protection range, in price points *)
  ; unit_of_measure : string (** MDP [UnitOfMeasure], e.g. ["IPNT"], ["BBL"] *)
  ; max_trade_vol : int
  }
[@@deriving sexp_of]

(** ESZ6, NQZ6, CLZ6, GCZ6 with security ids 1001 to 1004. *)
val all : t list

(** Globex symbol, e.g. ["ESZ6"]. *)
val symbol : t -> string

val find_symbol : string -> t option
val find_security_id : int -> t option
val tick : t -> Price.t
val multiplier : t -> int

(** The engine instrument: whole contracts, the product tick. *)
val instrument : t -> Exchange_core.Instrument.t

val engine_symbol : t -> Exchange_core.Symbol.t

(** Low and high price limits around [reference], rounded inward to the tick. *)
val price_limits : t -> reference:Price.t -> Price.t * Price.t

(** MDP [SecurityGroup] and [Asset]: the product root. *)
val security_group : t -> string

(** Last trading day, e.g. 2026-12-18 for ESZ6. *)
val last_trade_date : t -> Date.t
