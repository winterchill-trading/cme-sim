(** The Globex matching front end: CME order semantics on top of the venue-neutral engine.

    Requests arrive as decoded iLink 3 messages from an account (one per FIXP session);
    responses come back as iLink 3 business messages addressed to an account, with
    [SeqNum] and [UUID] left at zero for the session to fill in. Market data consumers get
    trades, book changes, phase changes and settlements. Everything is synchronous: call
    {!drain} after each request, phase change or {!sync}.

    On top of the engine this module keeps the CME order attributes the engine has no
    notion of: a stable OrderID across replaces, day versus GTC orders, fill and kill
    versus fill or kill, minimum quantity, iceberg display quantity (each refreshed slice
    joins the back of the queue), market orders with protection, price limits and the
    trading phase. *)

open! Core
module Price := Exchange_core.Price
module Ilink3 := Cme_sim_wire.Ilink3

module Phase : sig
  type t =
    | Open (** continuous trading *)
    | Pre_open (** orders accepted, not matched; uncrossed at the open *)
    | Halted
    | Closed (** maintenance window or weekend *)
  [@@deriving sexp, compare, equal, enumerate]
end

module Trade : sig
  type t =
    { security_id : int
    ; px : Price.t
    ; size : int
    ; aggressor : Ilink3.Side.t option (** [None] for an auction trade *)
    ; md_trade_entry_id : int
    ; orders : (int * int) list (** order id and quantity of each participant *)
    ; time : int
    }
  [@@deriving sexp_of]
end

module Output : sig
  type t =
    | Report of
        { account : Exchange_core.Account.t
        ; message : Ilink3.Message.t
        }
    | Trade of Trade.t
    | Book_changed of { security_id : int }
    | Phase_changed of
        { security_id : int
        ; phase : Phase.t
        ; time : int
        }
    | Settlement of
        { security_id : int
        ; px : Price.t
        ; time : int
        }
  [@@deriving sexp_of]
end

type t

(** Starts with every product {!Phase.Open}. [deposit] is credited to each account on its
    first request (default one billion: margin is not what the simulator is about). *)
val create : ?deposit:Exchange_core.Usd.t -> unit -> t

(** The engine, e.g. to run noise traders on. *)
val exchange : t -> Exchange_core_engine.Exchange.t

val phase : t -> security_id:int -> Phase.t

(** Current price limits of a product (around the last settlement). *)
val price_limits : t -> security_id:int -> Price.t * Price.t

(** Last trade price, else the last settlement. *)
val last_price : t -> security_id:int -> Price.t

(** Advances the clock (nanoseconds since the epoch; never goes backwards). *)
val set_time : t -> int -> unit

val new_order : t -> account:Exchange_core.Account.t -> Ilink3.New_order_single.t -> unit
val cancel : t -> account:Exchange_core.Account.t -> Ilink3.Order_cancel_request.t -> unit

val replace
  :  t
  -> account:Exchange_core.Account.t
  -> Ilink3.Order_cancel_replace_request.t
  -> unit

(** Cancels every order of an account, reporting [ExecRestatementReason] 100 (cancel on
    disconnect). *)
val cancel_on_disconnect : t -> account:Exchange_core.Account.t -> unit

(** Processes engine activity caused by someone else (noise traders): fills of resting
    orders, book changes. *)
val sync : t -> unit

(** Changes a product's phase. Closing expires day orders and publishes a settlement;
    opening after a pre-open runs the opening auction over the orders collected. *)
val set_phase : t -> security_id:int -> Phase.t -> unit

(** Outputs since the last call, oldest first. *)
val drain : t -> Output.t list

module Order_view : sig
  type t =
    { order_id : int
    ; cl_ord_id : string
    ; security_id : int
    ; side : Ilink3.Side.t
    ; price : Price.t
    ; order_qty : int
    ; cum_qty : int
    ; display_qty : int option
    ; queued : bool (** entered in pre-open, not yet in the book *)
    }
  [@@deriving sexp_of]
end

(** Working orders of an account, by order id. *)
val orders : t -> account:Exchange_core.Account.t -> Order_view.t list
