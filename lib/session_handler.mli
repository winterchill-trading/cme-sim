(** The exchange side of one iLink 3 TCP connection: the FIXP session state machine.

    The handler is pure: it takes decoded messages and clock ticks and returns actions for
    the connection driver. Sequencing state lives in a {!Registry} entry per session (firm
    and session id) that outlives connections, so a client that reconnects with the same
    UUID continues its sequence numbers and can ask for a retransmission of what it
    missed, including the reports sent while it was away.

    Rules implemented:
    - Negotiate (HMAC accepted unchecked) then Establish, which must come within 60 s of
      the connection; anything else first is answered with a reject or Terminate.
    - Customer sequence numbers: a gap is reported with NotApplied and the message is
      processed; a number below the expected one terminates the session.
    - Keep-alive: the exchange sends Sequence when it has been silent for the interval;
      when the client has been silent for one interval it gets a Sequence with
      KeepAliveIntervalLapsed set, after a second interval the session is terminated.
    - RetransmitRequest replays stored business messages with PossRetransFlag set, at most
      2500 per request. *)

open! Core
module Ilink3 := Cme_sim_wire.Ilink3

module Registry : sig
  type t

  val create : unit -> t
end

module Action : sig
  type t =
    | Send of Ilink3.Message.t
    | Business of
        { account : Exchange_core.Account.t
        ; message : Ilink3.Message.t
        } (** a client request for the venue *)
    | Disconnect of string
  [@@deriving sexp_of]
end

type t

val create : Registry.t -> now:Time_ns.t -> t

(** Firm and session as an engine account, e.g. ["001/ABC"], once negotiated. *)
val account : t -> Exchange_core.Account.t option

val is_established : t -> bool
val on_message : t -> now:Time_ns.t -> Ilink3.Message.t -> Action.t list

(** Keep-alive and establishment deadlines; call about once a second. *)
val on_timer : t -> now:Time_ns.t -> Action.t list

(** Sequences an exchange business message for this session's client. *)
val send_business : t -> now:Time_ns.t -> Ilink3.Message.t -> Action.t list

(** Sequences and stores a business message for a session with no live connection, so it
    can be retransmitted after the client reconnects with the same UUID. *)
val store_offline
  :  Registry.t
  -> account:Exchange_core.Account.t
  -> Ilink3.Message.t
  -> unit
