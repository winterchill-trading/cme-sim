(** Request builders and one-line output printing shared by the venue tests. *)

open! Core
open Cme_sim
module Ilink3 := Cme_sim_wire.Ilink3

val alice : Exchange_core.Account.t
val bob : Exchange_core.Account.t

val order
  :  ?price:string
  -> ?tif:Ilink3.Time_in_force.t
  -> ?ord_type:Ilink3.Ord_type.t
  -> ?min_qty:int
  -> ?display_qty:int
  -> ?security_id:int
  -> Ilink3.Side.t
  -> int
  -> cl_ord_id:string
  -> Ilink3.New_order_single.t

val cancel_request : order_id:int -> cl_ord_id:string -> Ilink3.Order_cancel_request.t

val replace_request
  :  order_id:int
  -> cl_ord_id:string
  -> price:string
  -> qty:int
  -> Ilink3.Order_cancel_replace_request.t

(** One line per output. *)
val print_outputs : Venue.Output.t list -> unit

(** Drains and prints. *)
val show : Venue.t -> unit

(** Order id of the last ExecutionReportNew in the outputs. *)
val last_order_id : Venue.Output.t list -> int
