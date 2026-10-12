(** Which trading phase the simulated Globex is in.

    - [Always_open]: continuous trading, the default for playing with the simulator.
    - [Real]: the CME Globex week from {!Equities_core.Session.Cme_globex} (Sunday 17:00
      to Friday 16:00 Chicago time with the daily 16:00 to 17:00 maintenance window); the
      last 15 minutes before each open are pre-open.
    - [Accelerated]: the same cycle compressed to a few minutes: pre-open, open,
      maintenance, repeated from the server start. *)

open! Core

module Mode : sig
  type t =
    | Real
    | Accelerated
    | Always_open
  [@@deriving sexp, compare, equal, enumerate]

  val of_string : string -> t
  val arg_type : t Command.Arg_type.t
end

(** Accelerated cycle lengths. *)
val accelerated_pre_open : Time_ns.Span.t

val accelerated_open : Time_ns.Span.t
val accelerated_closed : Time_ns.Span.t
val pre_open_before_real_open : Time_ns.Span.t
val phase : Mode.t -> start:Time_ns.t -> now:Time_ns.t -> Venue.Phase.t
