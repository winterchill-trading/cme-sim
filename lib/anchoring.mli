(** Ties a product's noise traders to a reference price (the TradFi path of the price
    anchor design).

    There is no real-time CME data to shadow or replay, so the noise traders stay the
    book's only liquidity. Their random walk becomes mean-reverting around a reference
    price: a {!Target.t} gives the reference at any simulated time, and while the product
    is {!Venue.Phase.Open} each step pulls the walk towards it through
    [Market_sim.set_reference ~half_life]; the mark follows on the next noise tick.

    In the other phases (pre-open, a limit halt, the maintenance window) the venue owns
    the price: the anchor neither pulls nor ticks the walk, and only counts the wait. When
    the product opens again the walk restarts from the venue's last price (the opening
    auction's, if it traded) and is pulled back towards the reference from there. Daily
    price limits keep coming from the settlement, i.e. from the simulator's own trades. *)

open! Core
module Price := Exchange_core.Price
module Market_sim := Exchange_core_engine.Market_sim

module Target : sig
  type t =
    | Series of (Time_ns.t * Price.t) array
    (** recorded [(time, price)] points in time order; the first point is mapped to the
        simulator's start and the series is then followed in real time, holding the last
        point once it ends *)
    | Fixed of
        { price : Price.t
        ; drift_bps_per_day : float (** compounded *)
        }
  [@@deriving sexp_of]

  (** The reference price [elapsed] after the simulator started; [None] for an empty
      series or a non-positive price. *)
  val price_at : t -> elapsed:Time_ns.Span.t -> Price.t option
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
    (trades, BBO mids and L2 book mids, in [recv_ts] order) scaled by its factor. *)
val load
  :  ?norm:Norm_range.t
  -> Spec.t
  -> (Products.t * Target.t) Or_error.t Async.Deferred.t

module Stats : sig
  type t =
    { pulls : int (** steps that pulled the walk towards the reference *)
    ; waits : int (** steps spent outside the open phase *)
    ; restarts : int (** reopenings that restarted the walk from the venue price *)
    ; last_target : Price.t option (** the last reference, rounded to the tick *)
    }
  [@@deriving sexp_of]
end

type t

(** [half_life] defaults to one second. [start] is the simulated time the targets count
    from. *)
val create
  :  ?half_life:Time_ns.Span.t
  -> start:Time_ns.t
  -> (Products.t * Target.t) list
  -> t

val is_anchored : t -> Products.t -> bool

(** The reference of an anchored product at [now], rounded to its tick. *)
val target : t -> Products.t -> now:Time_ns.t -> Price.t option

(** Called before each noise tick, with the product's current phase. Does nothing for a
    product without an anchor. *)
val step : t -> Products.t -> Market_sim.t -> venue:Venue.t -> now:Time_ns.t -> unit

val stats : t -> Products.t -> Stats.t option
