open! Core
open Cme_sim_wire
module Price = Exchange_core.Price

let buf () = Bigstring.init 16 ~f:(fun _ -> '\000')

let%expect_test "integers are little endian" =
  let b = buf () in
  Sbe.set_u16 b ~pos:0 0xCAFE;
  Sbe.set_u32 b ~pos:2 0x01020304;
  Sbe.set_i32 b ~pos:6 (-2);
  Sbe.set_u8 b ~pos:10 255;
  print_endline (Sbe.hex b);
  [%expect {| fe ca 04 03 02 01 fe ff ff ff ff 00 00 00 00 00 |}];
  print_s
    [%message
      ""
        ~u16:(Sbe.get_u16 b ~pos:0 : int)
        ~u32:(Sbe.get_u32 b ~pos:2 : int)
        ~i32:(Sbe.get_i32 b ~pos:6 : int)];
  [%expect {| ((u16 51966) (u32 16909060) (i32 -2)) |}]
;;

let%expect_test "null values" =
  let b = buf () in
  Sbe.set_u32_opt b ~pos:0 None;
  Sbe.set_u64_opt b ~pos:4 None;
  Sbe.set_u8_opt b ~pos:12 None;
  Sbe.set_char_opt b ~pos:13 None;
  print_endline (Sbe.hex b);
  [%expect {| ff ff ff ff ff ff ff ff ff ff ff ff ff 00 00 00 |}];
  print_s
    [%message
      ""
        ~u32:(Sbe.get_u32_opt b ~pos:0 : int option)
        ~u64:(Sbe.get_u64_opt b ~pos:4 : int option)
        ~u8:(Sbe.get_u8_opt b ~pos:12 : int option)
        ~c:(Sbe.get_char_opt b ~pos:13 : char option)];
  [%expect {| ((u32 ()) (u64 ()) (u8 ()) (c ())) |}]
;;

let%expect_test "PRICE9: 100000000000 is 100.0" =
  let b = buf () in
  Sbe.set_price b ~pos:0 (Price.of_string "100");
  Sbe.set_price_opt b ~pos:8 None;
  print_endline (Sbe.hex b);
  [%expect {| 00 e8 76 48 17 00 00 00 ff ff ff ff ff ff ff 7f |}];
  print_s
    [%sexp
      (Sbe.get_i64 b ~pos:0 : Int64.t), (Sbe.get_price_opt b ~pos:8 : Price.t option)];
  [%expect {| (100000000000 ()) |}]
;;

let%test_unit "price round trip" =
  Quickcheck.test (Int.gen_incl (-1_000_000_000_000) 1_000_000_000_000) ~f:(fun units ->
    let p = Price.of_units units in
    let b = buf () in
    Sbe.set_price b ~pos:3 p;
    [%test_result: Price.t] (Sbe.get_price b ~pos:3) ~expect:p)
;;

let%test_unit "unsigned round trips" =
  Quickcheck.test
    (Quickcheck.Generator.tuple3
       (Int.gen_incl 0 65534)
       (Int.gen_incl 0 4294967294)
       (Int.gen_incl 0 Int.max_value))
    ~f:(fun (a, b', c) ->
      let b = buf () in
      Sbe.set_u16_opt b ~pos:0 (Some a);
      Sbe.set_u32_opt b ~pos:2 (Some b');
      Sbe.set_u64_opt b ~pos:6 (Some c);
      [%test_result: int option] (Sbe.get_u16_opt b ~pos:0) ~expect:(Some a);
      [%test_result: int option] (Sbe.get_u32_opt b ~pos:2) ~expect:(Some b');
      [%test_result: int option] (Sbe.get_u64_opt b ~pos:6) ~expect:(Some c))
;;

let%expect_test "strings are NUL padded" =
  let b = buf () in
  Sbe.set_string b ~pos:0 ~len:8 "ESZ6";
  print_endline (Sbe.hex b);
  [%expect {| 45 53 5a 36 00 00 00 00 00 00 00 00 00 00 00 00 |}];
  print_endline (Sbe.get_string b ~pos:0 ~len:8);
  [%expect {| ESZ6 |}]
;;

let%expect_test "headers and groups" =
  let b = Bigstring.init 20 ~f:(fun _ -> '\000') in
  Sbe.Sofh.write b ~pos:0 ~message_size:128;
  Sbe.Header.write
    b
    ~pos:4
    { block_length = 116; template_id = 514; schema_id = 8; version = 5 };
  let next = Sbe.Group.write ~eight_byte:true b ~pos:12 ~block_length:24 2 in
  print_endline (Sbe.hex b);
  print_s
    [%message
      ""
        ~next:(next : int)
        ~sofh:(Sbe.Sofh.read b ~pos:0 : int Or_error.t)
        ~header:(Sbe.Header.read b ~pos:4 : Sbe.Header.t)
        ~group:(Sbe.Group.read ~eight_byte:true b ~pos:12 : int * int * int)];
  [%expect
    {|
    80 00 fe ca 74 00 02 02 08 00 05 00 18 00 00 00
    00 00 00 02
    ((next 20) (sofh (Ok 128))
     (header ((block_length 116) (template_id 514) (schema_id 8) (version 5)))
     (group (24 2 20)))
    |}]
;;
