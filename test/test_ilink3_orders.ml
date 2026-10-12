open! Core
open Cme_sim_wire
open Ilink3
module Price = Exchange_core.Price

let new_order : New_order_single.t =
  { price = Some (Price.of_string "6000.25")
  ; order_qty = 3
  ; security_id = 1001
  ; side = Buy
  ; seq_num = 7
  ; sender_id = "TRADER1"
  ; cl_ord_id = "ORD-1"
  ; party_details_list_req_id = 1
  ; order_request_id = 11
  ; sending_time = 1_760_000_000_000_000_000
  ; stop_px = None
  ; location = "US,IL"
  ; min_qty = None
  ; display_qty = Some 1
  ; ord_type = Limit
  ; time_in_force = Day
  ; manual = false
  }
;;

let%expect_test "NewOrderSingle: offsets from the spec summary" =
  let b = encode (New_order_single new_order) in
  let at off = 12 + off in
  print_s
    [%message
      ""
        ~size:(Bigstring.length b : int)
        ~header:(Sbe.Header.read b ~pos:4 : Sbe.Header.t)
        ~price_mantissa:(Sbe.get_i64 b ~pos:(at 0) : Int64.t)
        ~order_qty:(Sbe.get_u32 b ~pos:(at 8) : int)
        ~security_id:(Sbe.get_i32 b ~pos:(at 12) : int)
        ~side:(Sbe.get_u8 b ~pos:(at 16) : int)
        ~seq_num:(Sbe.get_u32 b ~pos:(at 17) : int)
        ~sender_id:(Sbe.get_string b ~pos:(at 21) ~len:20 : string)
        ~cl_ord_id:(Sbe.get_string b ~pos:(at 41) ~len:20 : string)
        ~stop_px:(Sbe.get_price_opt b ~pos:(at 85) : Price.t option)
        ~location:(Sbe.get_string b ~pos:(at 93) ~len:5 : string)
        ~min_qty:(Sbe.get_u32_opt b ~pos:(at 98) : int option)
        ~display_qty:(Sbe.get_u32_opt b ~pos:(at 102) : int option)
        ~ord_type:(Sbe.get_char b ~pos:(at 108) : char)
        ~tif:(Sbe.get_u8 b ~pos:(at 109) : int)];
  [%expect
    {|
    ((size 128)
     (header ((block_length 116) (template_id 514) (schema_id 8) (version 5)))
     (price_mantissa 6000250000000) (order_qty 3) (security_id 1001) (side 1)
     (seq_num 7) (sender_id TRADER1) (cl_ord_id ORD-1) (stop_px ())
     (location US,IL) (min_qty ()) (display_qty (1)) (ord_type 2) (tif 0))
    |}];
  (* the full frame, for comparison with captures *)
  print_endline (Sbe.hex b);
  [%expect
    {|
    80 00 fe ca 74 00 02 02 08 00 05 00 80 12 c5 0a
    75 05 00 00 03 00 00 00 e9 03 00 00 01 07 00 00
    00 54 52 41 44 45 52 31 00 00 00 00 00 00 00 00
    00 00 00 00 00 4f 52 44 2d 31 00 00 00 00 00 00
    00 00 00 00 00 00 00 00 00 01 00 00 00 00 00 00
    00 0b 00 00 00 00 00 00 00 00 00 b0 d4 ac c6 6c
    18 ff ff ff ff ff ff ff 7f 55 53 2c 49 4c ff ff
    ff ff 01 00 00 00 ff ff 32 00 00 00 00 ff ff ff
    |}]
;;

let round_trip m =
  match decode (encode m) with
  | Ok m' when Message.equal m m' -> ()
  | other ->
    raise_s [%message "round trip failed" (m : Message.t) (other : Message.t Or_error.t)]
;;

let%expect_test "requests round trip" =
  round_trip (New_order_single new_order);
  round_trip
    (New_order_single
       { new_order with
         price = None
       ; ord_type = Market_with_protection
       ; time_in_force = Fak
       ; min_qty = Some 2
       ; display_qty = None
       ; side = Sell
       ; manual = true
       });
  round_trip
    (Order_cancel_replace_request
       { price = Some (Price.of_string "71.23")
       ; order_qty = 5
       ; security_id = 3001
       ; side = Sell
       ; seq_num = 8
       ; sender_id = "TRADER1"
       ; cl_ord_id = "ORD-2"
       ; party_details_list_req_id = 1
       ; order_id = 123456789
       ; stop_px = None
       ; order_request_id = 12
       ; sending_time = 5
       ; location = "US,IL"
       ; min_qty = None
       ; display_qty = None
       ; ord_type = Limit
       ; time_in_force = Gtc
       ; manual = false
       ; ofm_override = true
       });
  round_trip
    (Order_cancel_request
       { order_id = 123456789
       ; party_details_list_req_id = 1
       ; manual = false
       ; seq_num = 9
       ; sender_id = "TRADER1"
       ; cl_ord_id = "ORD-3"
       ; order_request_id = 13
       ; sending_time = 6
       ; location = "US,IL"
       ; security_id = 3001
       ; side = Sell
       });
  [%expect {| |}]
;;

let%expect_test "OrderCancelRequest and replace offsets" =
  let b =
    encode
      (Order_cancel_request
         { order_id = 77
         ; party_details_list_req_id = 1
         ; manual = false
         ; seq_num = 9
         ; sender_id = "S"
         ; cl_ord_id = "C"
         ; order_request_id = 13
         ; sending_time = 6
         ; location = "US,IL"
         ; security_id = 3001
         ; side = Sell
         })
  in
  let at off = 12 + off in
  print_s
    [%message
      ""
        ~block:(Sbe.get_u16 b ~pos:4 : int)
        ~order_id:(Sbe.get_u64 b ~pos:(at 0) : int)
        ~seq_num:(Sbe.get_u32 b ~pos:(at 17) : int)
        ~security_id:(Sbe.get_i32 b ~pos:(at 82) : int)
        ~side:(Sbe.get_u8 b ~pos:(at 86) : int)];
  [%expect {| ((block 88) (order_id 77) (seq_num 9) (security_id 3001) (side 2)) |}]
;;

let%expect_test "a longer root block from a newer schema still decodes" =
  let b = encode (New_order_single new_order) in
  let longer = Bigstring.init (Bigstring.length b + 4) ~f:(fun _ -> '\000') in
  Bigstring.blit ~src:b ~src_pos:0 ~dst:longer ~dst_pos:0 ~len:(Bigstring.length b);
  Sbe.Sofh.write longer ~pos:0 ~message_size:(Bigstring.length longer);
  Sbe.set_u16 longer ~pos:4 120;
  print_s
    [%sexp
      (Or_error.map (decode longer) ~f:(Message.equal (New_order_single new_order))
       : bool Or_error.t)];
  [%expect {| (Ok true) |}]
;;

let%expect_test "invalid enum values are decode errors" =
  let b = encode (New_order_single new_order) in
  Sbe.set_u8 b ~pos:(12 + 16) 9;
  print_s [%sexp (decode b : Message.t Or_error.t)];
  [%expect {| (Error "invalid Side 9") |}]
;;
