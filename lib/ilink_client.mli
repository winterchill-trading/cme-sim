(** A minimal iLink 3 client over TCP: negotiates and establishes a FIXP session, numbers
    business messages, keeps the session alive with Sequence messages and delivers what
    the exchange sends. Used by the CLI and the tests; it also works as a starting point
    for strategies talking to the simulator. *)

open! Core
open! Async
module Ilink3 := Cme_sim_wire.Ilink3

type t

(** Connects, sends Negotiate and Establish and waits for both acks. [uuid] defaults to
    the current time in microseconds. *)
val connect
  :  ?firm:string
  -> ?session:string
  -> ?uuid:int
  -> ?keep_alive_ms:int
  -> host:string
  -> port:int
  -> unit
  -> t Deferred.Or_error.t

val uuid : t -> int

(** The EstablishmentAck received. *)
val establishment : t -> Ilink3.Establishment_ack.t

(** Messages from the exchange after the EstablishmentAck, Sequence heartbeats included. *)
val messages : t -> Ilink3.Message.t Pipe.Reader.t

(** Sends a business message built for the next sequence number. *)
val send : t -> (seq_num:int -> Ilink3.Message.t) -> unit

(** Sends a session message (e.g. RetransmitRequest) as is. *)
val send_raw : t -> Ilink3.Message.t -> unit

(** Sends Terminate and closes the connection. *)
val close : t -> unit Deferred.t

(** {2 Request builders} *)

val new_order
  :  ?price:Exchange_core.Price.t
  -> ?tif:Ilink3.Time_in_force.t
  -> ?ord_type:Ilink3.Ord_type.t
  -> ?display_qty:int
  -> ?min_qty:int
  -> security_id:int
  -> side:Ilink3.Side.t
  -> qty:int
  -> cl_ord_id:string
  -> seq_num:int
  -> unit
  -> Ilink3.Message.t

val cancel
  :  order_id:int
  -> security_id:int
  -> side:Ilink3.Side.t
  -> cl_ord_id:string
  -> seq_num:int
  -> Ilink3.Message.t

val replace
  :  ?tif:Ilink3.Time_in_force.t
  -> order_id:int
  -> security_id:int
  -> side:Ilink3.Side.t
  -> price:Exchange_core.Price.t
  -> qty:int
  -> cl_ord_id:string
  -> seq_num:int
  -> unit
  -> Ilink3.Message.t

(** One line describing a message, for logs and the CLI. *)
val describe : Ilink3.Message.t -> string
