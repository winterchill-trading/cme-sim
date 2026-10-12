(** Ties a product's noise traders to a reference price (the TradFi path of the price
    anchor design), one {!Price_anchor.Anchor} per anchored product.

    There is no real-time CME data to shadow or replay, so the noise traders stay the
    book's only liquidity. Their random walk becomes mean-reverting around a reference
    price: a {!Price_anchor.Path} pulled on the venue's clock, and while the product is
    {!Venue.Phase.Open} each step pulls the walk towards it through
    [Market_sim.set_reference ~half_life]; the mark follows on the next noise tick. The
    target is kept 30 bps inside the product's daily price limits
    ({!Price_anchor.Clamp.within_bands}), so the anchor alone never drives the product
    into a limit halt.

    In the other phases (pre-open, a limit halt, the maintenance window) the venue owns
    the price: the anchor hands the symbol back to the free walk (which does not tick
    outside the open) and counts the wait. When the product opens again the walk restarts
    from the venue's last price (the opening auction's, if it traded) and is pulled back
    towards the reference from there. Daily price limits keep coming from the settlement,
    i.e. from the simulator's own trades. *)

open! Core
module Price := Exchange_core.Price
module Market_sim := Exchange_core_engine.Market_sim

(** Where an anchored product's reference comes from: a path of reference prices and the
    factor that scales them to the product ([Anchor]'s [price_factor]). *)
module Reference : sig
  type t =
    { path : Price_anchor.Path.t
    ; price_factor : float
    }
  [@@deriving sexp_of]
end

(** A parsed [-anchor-*] flag. *)
module Spec : sig
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

  (** [FILE=SYMBOL]: a series file of [<time> <price>] lines ({!Price_anchor.Source}). *)
  val series_of_string : string -> t Or_error.t

  (** [PRICE[,DRIFT_BPS_PER_DAY]=SYMBOL]. *)
  val fixed_of_string : string -> t Or_error.t

  (** [VENUE:SYMBOL=SIM_SYMBOL,FACTOR]: a recorded reference instrument, its prices
      multiplied by [FACTOR]. *)
  val scaled_of_string : string -> t Or_error.t

  val arg_type : (string -> t Or_error.t) -> t Command.Arg_type.t
end

module Norm_range : sig
  (** Where [Scaled] specs read their reference: normalized event logs under [dir] for the
      UTC days [[from, to_]]. *)
  type t =
    { dir : string
    ; from : Date.t
    ; to_ : Date.t
    }
  [@@deriving sexp_of]
end

(** Reads the points of a series file, or the reference prices of a recorded instrument
    (book mids, L2 or BBO, else trades: {!Price_anchor.Source.to_path}) with its factor. *)
val load
  :  ?norm:Norm_range.t
  -> Spec.t
  -> (Products.t * Reference.t) Or_error.t Async.Deferred.t

module Stats : sig
  type t =
    { pulls : int (** reference changes passed to the walk ([Anchor] [marks_set]) *)
    ; waits : int (** steps spent outside the open phase *)
    ; restarts : int (** reopenings that restarted the walk from the venue price *)
    ; clamped : int (** pulls whose target the daily price limits moved *)
    ; last_target : Price.t option
    (** the last reference passed to the walk, after the clamp; [None] from a reopening
        until the next pull *)
    }
  [@@deriving sexp_of]
end

type t

(** One anchor per [(product, reference, sim)], [sim] being the product's noise traders
    (it keeps its own noise trader count). [half_life] defaults to one second. The paths
    start at the venue's current time, so set it first. [log] receives the anchors' pause
    and resume lines (default: dropped). *)
val create
  :  ?half_life:Time_ns.Span.t
  -> ?log:(Sexp.t -> unit)
  -> Venue.t
  -> (Products.t * Reference.t * Market_sim.t) list
  -> t

val is_anchored : t -> Products.t -> bool

(** The reference of an anchored product at the last step, rounded to its tick, before the
    clamp. *)
val target : t -> Products.t -> Price.t option

(** Called before each noise tick, after the venue's time is set. Does nothing for a
    product without an anchor. *)
val step : t -> Products.t -> unit

val stats : t -> Products.t -> Stats.t option
