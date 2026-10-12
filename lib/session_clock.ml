open! Core
module Globex = Equities_core.Session.Cme_globex

module Mode = struct
  type t =
    | Real
    | Accelerated
    | Always_open
  [@@deriving sexp, compare, equal, enumerate]

  let of_string s =
    match String.lowercase s with
    | "real" -> Real
    | "accelerated" -> Accelerated
    | "always-open" | "always_open" -> Always_open
    | _ -> raise_s [%message "clock must be real, accelerated or always-open" s]
  ;;

  let arg_type = Command.Arg_type.create of_string
end

let accelerated_pre_open = Time_ns.Span.of_sec 20.
let accelerated_open = Time_ns.Span.of_sec 120.
let accelerated_closed = Time_ns.Span.of_sec 20.
let pre_open_before_real_open = Time_ns.Span.of_min 15.

let accelerated ~start ~now : Venue.Phase.t =
  let cycle =
    Time_ns.Span.(accelerated_pre_open + accelerated_open + accelerated_closed)
  in
  let elapsed = Time_ns.Span.max Time_ns.Span.zero (Time_ns.diff now start) in
  let into =
    Time_ns.Span.of_int_ns (Time_ns.Span.to_int_ns elapsed % Time_ns.Span.to_int_ns cycle)
  in
  if Time_ns.Span.( < ) into accelerated_pre_open
  then Pre_open
  else if Time_ns.Span.( < ) into Time_ns.Span.(accelerated_pre_open + accelerated_open)
  then Open
  else Closed
;;

let real ~now : Venue.Phase.t =
  match Globex.phase_at now with
  | Open -> Open
  | Maintenance | Closed ->
    (match Globex.phase_at (Time_ns.add now pre_open_before_real_open) with
     | Open -> Pre_open
     | Maintenance | Closed -> Closed)
;;

let phase (mode : Mode.t) ~start ~now =
  match mode with
  | Always_open -> Venue.Phase.Open
  | Accelerated -> accelerated ~start ~now
  | Real -> real ~now
;;
