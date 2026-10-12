open! Core
open Cme_sim
module Ilink3 = Cme_sim_wire.Ilink3
module Price = Exchange_core.Price

let alice = Exchange_core.Account.of_string "001/ALC"
let bob = Exchange_core.Account.of_string "002/BOB"

let order
  ?price
  ?(tif = Ilink3.Time_in_force.Day)
  ?(ord_type = Ilink3.Ord_type.Limit)
  ?min_qty
  ?display_qty
  ?(security_id = 1001)
  side
  qty
  ~cl_ord_id
  : Ilink3.New_order_single.t
  =
  { price = Option.map price ~f:Price.of_string
  ; order_qty = qty
  ; security_id
  ; side
  ; seq_num = 1
  ; sender_id = "T1"
  ; cl_ord_id
  ; party_details_list_req_id = 1
  ; order_request_id = 1
  ; sending_time = 0
  ; stop_px = None
  ; location = "US,IL"
  ; min_qty
  ; display_qty
  ; ord_type
  ; time_in_force = tif
  ; manual = false
  }
;;

let cancel_request ~order_id ~cl_ord_id : Ilink3.Order_cancel_request.t =
  { order_id
  ; party_details_list_req_id = 1
  ; manual = false
  ; seq_num = 1
  ; sender_id = "T1"
  ; cl_ord_id
  ; order_request_id = 2
  ; sending_time = 0
  ; location = "US,IL"
  ; security_id = 1001
  ; side = Buy
  }
;;

let replace_request ~order_id ~cl_ord_id ~price ~qty
  : Ilink3.Order_cancel_replace_request.t
  =
  { price = Some (Price.of_string price)
  ; order_qty = qty
  ; security_id = 1001
  ; side = Buy
  ; seq_num = 1
  ; sender_id = "T1"
  ; cl_ord_id
  ; party_details_list_req_id = 1
  ; order_id
  ; stop_px = None
  ; order_request_id = 3
  ; sending_time = 0
  ; location = "US,IL"
  ; min_qty = None
  ; display_qty = None
  ; ord_type = Limit
  ; time_in_force = Day
  ; manual = false
  ; ofm_override = false
  }
;;

let px = Price.to_string

let describe (m : Ilink3.Message.t) =
  match m with
  | Execution_report_new r ->
    sprintf
      "New %s %s %d @ %s"
      r.common.cl_ord_id
      (Sexp.to_string [%sexp (r.common.side : Ilink3.Side.t)])
      r.order_qty
      (px r.price)
  | Execution_report_reject r ->
    sprintf "Reject %s reason %d: %s" r.common.cl_ord_id r.ord_rej_reason r.text
  | Execution_report_trade_outright r ->
    sprintf
      "Trade %s %d @ %s cum %d leaves %d%s"
      r.common.cl_ord_id
      r.last_qty
      (px r.last_px)
      r.cum_qty
      r.leaves_qty
      (if r.aggressor then " aggressor" else "")
  | Execution_report_elimination r ->
    sprintf "Elimination %s cum %d of %d" r.common.cl_ord_id r.cum_qty r.order_qty
  | Execution_report_cancel r ->
    sprintf
      "Cancel %s cum %d%s"
      r.common.cl_ord_id
      r.cum_qty
      (Option.value_map r.exec_restatement_reason ~default:"" ~f:(sprintf " reason %d"))
  | Execution_report_modify r ->
    sprintf
      "Modify %s %d @ %s leaves %d"
      r.common.cl_ord_id
      r.order_qty
      (px r.price)
      r.leaves_qty
  | Order_cancel_reject r -> sprintf "CancelReject %s: %s" r.cl_ord_id r.text
  | Order_cancel_replace_reject r -> sprintf "ReplaceReject %s: %s" r.cl_ord_id r.text
  | other -> Ilink3.Message.name other
;;

let print_outputs outputs =
  List.iter outputs ~f:(fun (o : Venue.Output.t) ->
    match o with
    | Report { account; message } ->
      printf "%s %s\n" (Exchange_core.Account.to_string account) (describe message)
    | Trade t ->
      printf
        "  trade %d: %d @ %s aggressor %s orders %d\n"
        t.security_id
        t.size
        (px t.px)
        (Sexp.to_string [%sexp (t.aggressor : Ilink3.Side.t option)])
        (List.length t.orders)
    | Book_changed { security_id } -> printf "  book %d\n" security_id
    | Phase_changed { security_id; phase; _ } ->
      printf
        "  phase %d %s\n"
        security_id
        (Sexp.to_string [%sexp (phase : Venue.Phase.t)])
    | Settlement { security_id; px = p; _ } ->
      printf "  settlement %d %s\n" security_id (px p))
;;

let show v = print_outputs (Venue.drain v)

let last_order_id outputs =
  List.filter_map outputs ~f:(fun (o : Venue.Output.t) ->
    match o with
    | Report { message = Execution_report_new r; _ } -> Some r.common.order_id
    | _ -> None)
  |> List.last_exn
;;
