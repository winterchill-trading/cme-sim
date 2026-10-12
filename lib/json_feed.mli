(** The MDP feed as JSON, for websocket consumers that do not want to parse SBE.

    Each MDP message becomes one object with the packet's [seq] and [time], a [type] and
    the message fields under readable names; prices are decimal strings and instruments
    carry their Globex symbol. *)

open! Core

val of_packet : Cme_sim_wire.Mdp3.Packet.t -> Yojson.Safe.t list
