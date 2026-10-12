(** The simulator process: the iLink 3 gateway, the MDP 3.0 TCP feed and an HTTP port with
    the JSON websocket ([/ws]), all driven by one {!Venue}.

    Timers advance the session clock (phases, opening auctions, day order expiry and
    settlement), run the noise traders while a product is open, send feed heartbeats and
    halt a product for [halt_duration] when a trade reaches its price limit.
    [GET /admin/halt?symbol=ESZ6&seconds=30] halts a product by hand; the halt ends with a
    short pre-open and a reopening auction. *)

open! Core
open! Async

module Config : sig
  type t =
    { clock : Session_clock.Mode.t
    ; noise : bool
    ; noise_symbols : string list (** empty: every product *)
    ; seed : int
    ; cancel_on_disconnect : bool
    ; feed_heartbeat : Time_ns.Span.t
    ; halt_duration : Time_ns.Span.t
    ; reopen_pre_open : Time_ns.Span.t (** pre-open before a halt reopens *)
    }
  [@@deriving sexp_of]

  (** Always open, noise on, seed 0, cancel on disconnect, 5 s feed heartbeat, 30 s halts
      with a 5 s pre-open. *)
  val default : t
end

type t

val create : Config.t -> t
val venue : t -> Venue.t
val feed : t -> Feed.t

module Ports : sig
  type t =
    { gateway : int
    ; feed : int
    ; http : int
    }
  [@@deriving sexp_of]
end

(** Binds the three ports (0 picks a free one) and starts the timers. *)
val start : t -> gateway_port:int -> feed_port:int -> http_port:int -> Ports.t Deferred.t

(** Halts a product now for [duration]. *)
val halt : t -> security_id:int -> duration:Time_ns.Span.t -> unit
