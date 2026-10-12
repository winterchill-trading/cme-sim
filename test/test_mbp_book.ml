open! Core
open Cme_sim_wire
module Price = Exchange_core.Price

let level px size : Mbp_book.Level.t = { px = Price.of_int px; size; orders = 1 }

let apply_all book updates =
  List.iteri updates ~f:(fun i (u : Mbp_book.Update.t) ->
    ok_exn (Mbp_book.apply book (Mbp_book.Update.to_entry u ~security_id:1 ~rpt_seq:i)))
;;

let%expect_test "diff of a small bid side" =
  let old = [ level 100 5; level 99 3; level 98 1 ] in
  let new_ = [ level 101 2; level 100 5; level 98 4 ] in
  let updates = Mbp_book.diff Bid ~old ~new_ in
  List.iter updates ~f:(fun (u : Mbp_book.Update.t) ->
    printf
      "%-6s level %d px %s size %d\n"
      (Sexp.to_string [%sexp (u.action : Mdp3.Update_action.t)])
      u.level
      (Price.to_string u.px)
      u.size);
  [%expect
    {|
    Delete level 2 px 99 size 3
    New    level 1 px 101 size 2
    Change level 3 px 98 size 4
    |}];
  let book = Mbp_book.create () in
  apply_all book (Mbp_book.diff Bid ~old:[] ~new_:old);
  apply_all book updates;
  print_s [%sexp (Mbp_book.bids book : Mbp_book.Level.t list)];
  [%expect
    {|
    (((px 101) (size 2) (orders 1)) ((px 100) (size 5) (orders 1))
     ((px 98) (size 4) (orders 1)))
    |}]
;;

(* A side: up to [depth] distinct prices, best first. *)
let gen_side ~depth ~descending =
  let open Quickcheck.Let_syntax in
  let%bind prices = List.gen_with_length 20 (Int.gen_incl 1 30) in
  let prices = List.dedup_and_sort prices ~compare:Int.compare in
  let prices = if descending then List.rev prices else prices in
  let%bind n = Int.gen_incl 0 depth in
  let prices = List.take prices n in
  let%map sizes = List.gen_with_length (List.length prices) (Int.gen_incl 1 3) in
  List.map2_exn prices sizes ~f:level
;;

let%test_unit "applying the diff reproduces the new book" =
  let depth = 5 in
  Quickcheck.test
    (Quickcheck.Generator.tuple3
       (gen_side ~depth ~descending:true)
       (gen_side ~depth ~descending:true)
       Bool.quickcheck_generator)
    ~f:(fun (old, new_, offer) ->
      let side : Mbp_book.Side.t = if offer then Offer else Bid in
      let book = Mbp_book.create ~depth () in
      apply_all book (Mbp_book.diff side ~old:[] ~new_:old);
      apply_all book (Mbp_book.diff side ~old ~new_);
      let got = if offer then Mbp_book.offers book else Mbp_book.bids book in
      [%test_result: Mbp_book.Level.t list] got ~expect:new_)
;;

let%expect_test "levels beyond the depth are dropped on insert" =
  let book = Mbp_book.create ~depth:2 () in
  apply_all book (Mbp_book.diff Offer ~old:[] ~new_:[ level 10 1; level 11 1 ]);
  apply_all
    book
    (Mbp_book.diff Offer ~old:[ level 10 1; level 11 1 ] ~new_:[ level 9 1; level 10 1 ]);
  print_s [%sexp (Mbp_book.offers book : Mbp_book.Level.t list)];
  [%expect {| (((px 9) (size 1) (orders 1)) ((px 10) (size 1) (orders 1))) |}];
  print_s
    [%sexp
      (Mbp_book.apply
         book
         { px = None
         ; size = None
         ; security_id = 1
         ; rpt_seq = 1
         ; number_of_orders = None
         ; price_level = 5
         ; update_action = Delete
         ; entry_type = Offer
         }
       : unit Or_error.t)];
  [%expect
    {| (Error ("book entry outside the book" (action Delete) (level 5) (levels 2))) |}]
;;
