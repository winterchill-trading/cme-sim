(** MDP 3.0 publisher for the simulated channel.

    Turns venue outputs into MDP packets, one message per packet:
    - trades become MDIncrementalRefreshTradeSummary;
    - book changes become MDIncrementalRefreshBook entries (market by price, 10 levels),
      computed by diffing the engine's book against the last published one;
    - phase changes become SecurityStatus;
    - settlements become MDIncrementalRefreshDailyStatistics.

    Every incremental packet takes the next channel [MsgSeqNum]; every book, trade and
    statistics entry takes the next [RptSeq] of its instrument. A new subscriber first
    gets the instrument definitions, the security status and a SnapshotFullRefresh of each
    instrument, in packets carrying the current [MsgSeqNum] (the snapshot's
    [LastMsgSeqNumProcessed]); incrementals with higher numbers follow. *)

open! Core
module Mdp3 := Cme_sim_wire.Mdp3

type t

val create : Venue.t -> t

(** MDP channel (ApplID) of every instrument. *)
val channel : int

val on_output : t -> now:int -> Venue.Output.t -> unit

(** Publishes an AdminHeartbeat if nothing was published for [interval]. *)
val heartbeat_if_idle : t -> now:int -> interval:Time_ns.Span.t -> unit

(** The last published [MsgSeqNum]. *)
val msg_seq_num : t -> int

(** Registers a subscriber and sends it the snapshot packets. Returns a function that
    unsubscribes. *)
val subscribe : t -> now:int -> (Mdp3.Packet.t -> unit) -> unit -> unit

(** The definition of one instrument as published at subscription. *)
val definition : t -> now:int -> Products.t -> Mdp3.Instrument_definition_future.t

val status_of_phase : Venue.Phase.t -> Mdp3.Trading_status.t
