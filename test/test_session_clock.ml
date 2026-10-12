open! Core
open Cme_sim

let chicago date hh mm =
  Equities_core.Session.Zone.of_local
    Chicago
    (Date.of_string date)
    (Equities_core.Session.hm hh mm)
;;

let%expect_test "real Globex week" =
  List.iter
    [ "2026-10-11", 16, 30 (* Sunday before the open *)
    ; "2026-10-11", 16, 50
    ; "2026-10-11", 17, 0
    ; "2026-10-12", 15, 59
    ; "2026-10-12", 16, 10 (* maintenance *)
    ; "2026-10-12", 16, 45
    ; "2026-10-16", 16, 50 (* Friday after the close *)
    ]
    ~f:(fun (date, hh, mm) ->
      let now = chicago date hh mm in
      printf
        "%s %02d:%02d CT %s\n"
        date
        hh
        mm
        (Sexp.to_string
           [%sexp (Session_clock.phase Real ~start:now ~now : Venue.Phase.t)]));
  [%expect
    {|
    2026-10-11 16:30 CT Closed
    2026-10-11 16:50 CT Pre_open
    2026-10-11 17:00 CT Open
    2026-10-12 15:59 CT Open
    2026-10-12 16:10 CT Closed
    2026-10-12 16:45 CT Pre_open
    2026-10-16 16:50 CT Closed
    |}]
;;

let%expect_test "accelerated cycle" =
  let start = Time_ns.epoch in
  List.iter [ 0.; 19.; 20.; 139.; 140.; 159.; 160.; 181. ] ~f:(fun sec ->
    let now = Time_ns.add start (Time_ns.Span.of_sec sec) in
    printf
      "t+%3g %s\n"
      sec
      (Sexp.to_string
         [%sexp (Session_clock.phase Accelerated ~start ~now : Venue.Phase.t)]));
  [%expect
    {|
    t+  0 Pre_open
    t+ 19 Pre_open
    t+ 20 Open
    t+139 Open
    t+140 Closed
    t+159 Closed
    t+160 Pre_open
    t+181 Open
    |}]
;;
