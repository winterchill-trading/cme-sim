open! Core
open Cme_sim_wire
open Ilink3

let show m =
  let b = encode m in
  let header = Sbe.Header.read b ~pos:4 in
  let back = decode b in
  print_s
    [%message
      (Message.name m)
        ~size:(Bigstring.length b : int)
        (header : Sbe.Header.t)
        ~round_trip:(Or_error.map back ~f:(Message.equal m) : bool Or_error.t)];
  b
;;

let negotiate : Message.t =
  Negotiate
    { hmac_signature = String.make 32 'h'
    ; access_key_id = "KEY1"
    ; uuid = 1_760_000_000_000
    ; request_timestamp = 1_760_000_000_123_456_789
    ; session = "ABC"
    ; firm = "001"
    }
;;

let%expect_test "Negotiate: offsets of the published layout" =
  let b = show negotiate in
  [%expect
    {|
    (Negotiate (size 90)
     (header ((block_length 76) (template_id 500) (schema_id 8) (version 5)))
     (round_trip (Ok true)))
    |}];
  (* body starts after SOFH (4) and header (8) *)
  let at off = 12 + off in
  print_s
    [%message
      ""
        ~sofh_size:(Sbe.get_u16 b ~pos:0 : int)
        ~encoding:(sprintf "0x%04X" (Sbe.get_u16 b ~pos:2) : string)
        ~access_key:(Sbe.get_string b ~pos:(at 32) ~len:20 : string)
        ~uuid:(Sbe.get_u64 b ~pos:(at 52) : int)
        ~request_timestamp:(Sbe.get_u64 b ~pos:(at 60) : int)
        ~session:(Sbe.get_string b ~pos:(at 68) ~len:3 : string)
        ~firm:(Sbe.get_string b ~pos:(at 71) ~len:5 : string)
        ~credentials_length:(Sbe.get_u16 b ~pos:(at 76) : int)];
  [%expect
    {|
    ((sofh_size 90) (encoding 0xCAFE) (access_key KEY1) (uuid 1760000000000)
     (request_timestamp 1760000000123456789) (session ABC) (firm 001)
     (credentials_length 0))
    |}]
;;

let%expect_test "Establish and EstablishmentAck" =
  let b =
    show
      (Establish
         { hmac_signature = ""
         ; access_key_id = "KEY1"
         ; trading_system_name = "cme-sim-cli"
         ; trading_system_version = "0.1"
         ; trading_system_vendor = "wtc"
         ; uuid = 42
         ; request_timestamp = 7
         ; next_seq_no = 1
         ; session = "ABC"
         ; firm = "001"
         ; keep_alive_interval = 30_000
         })
  in
  let at off = 12 + off in
  print_s
    [%message
      ""
        ~name:(Sbe.get_string b ~pos:(at 52) ~len:30 : string)
        ~uuid:(Sbe.get_u64 b ~pos:(at 102) : int)
        ~next_seq_no:(Sbe.get_u32 b ~pos:(at 118) : int)
        ~keep_alive:(Sbe.get_u16 b ~pos:(at 130) : int)];
  let ack =
    show
      (Establishment_ack
         { uuid = 42
         ; request_timestamp = 7
         ; next_seq_no = 1
         ; previous_seq_no = 0
         ; previous_uuid = 0
         ; keep_alive_interval = 30_000
         ; secret_key_secure_id_expiration = None
         })
  in
  print_endline (Sbe.hex ack);
  [%expect
    {|
    (Establish (size 146)
     (header ((block_length 132) (template_id 503) (schema_id 8) (version 5)))
     (round_trip (Ok true)))
    ((name cme-sim-cli) (uuid 42) (next_seq_no 1) (keep_alive 30000))
    (EstablishmentAck (size 50)
     (header ((block_length 38) (template_id 504) (schema_id 8) (version 5)))
     (round_trip (Ok true)))
    32 00 fe ca 26 00 f8 01 08 00 05 00 2a 00 00 00
    00 00 00 00 07 00 00 00 00 00 00 00 01 00 00 00
    00 00 00 00 00 00 00 00 00 00 00 00 30 75 ff ff
    ff ff
    |}]
;;

let%expect_test "Sequence is the heartbeat" =
  let b =
    show (Sequence { uuid = 42; next_seq_no = 17; keep_alive_interval_lapsed = true })
  in
  print_endline (Sbe.hex b);
  [%expect
    {|
    (Sequence (size 26)
     (header ((block_length 14) (template_id 506) (schema_id 8) (version 5)))
     (round_trip (Ok true)))
    1a 00 fe ca 0e 00 fa 01 08 00 05 00 2a 00 00 00
    00 00 00 00 11 00 00 00 ff 01
    |}]
;;

let%expect_test "other session messages round trip" =
  List.iter
    [ Message.Negotiation_response
        { uuid = 1
        ; request_timestamp = 2
        ; secret_key_secure_id_expiration = Some 30
        ; previous_seq_no = 3
        ; previous_uuid = 4
        }
    ; Negotiation_reject
        { reason = "bad credentials"; uuid = 1; request_timestamp = 2; error_codes = 3 }
    ; Establishment_reject
        { reason = "no negotiate"
        ; uuid = 1
        ; request_timestamp = 2
        ; next_seq_no = 5
        ; error_codes = 4
        }
    ; Terminate { reason = "bye"; uuid = 1; request_timestamp = 2; error_codes = 0 }
    ; Retransmit_request
        { uuid = 1
        ; last_uuid = None
        ; request_timestamp = 2
        ; from_seq_no = 10
        ; msg_count = 5
        }
    ; Retransmission
        { uuid = 1
        ; last_uuid = Some 9
        ; request_timestamp = 2
        ; from_seq_no = 10
        ; msg_count = 5
        }
    ; Retransmit_reject
        { reason = "too many"
        ; uuid = 1
        ; last_uuid = None
        ; request_timestamp = 2
        ; error_codes = 1
        }
    ; Not_applied { uuid = 1; from_seq_no = 4; msg_count = 2 }
    ]
    ~f:(fun m -> ignore (show m : Bigstring.t));
  [%expect
    {|
    (NegotiationResponse (size 46)
     (header ((block_length 32) (template_id 501) (schema_id 8) (version 5)))
     (round_trip (Ok true)))
    (NegotiationReject (size 80)
     (header ((block_length 68) (template_id 502) (schema_id 8) (version 5)))
     (round_trip (Ok true)))
    (EstablishmentReject (size 84)
     (header ((block_length 72) (template_id 505) (schema_id 8) (version 5)))
     (round_trip (Ok true)))
    (Terminate (size 79)
     (header ((block_length 67) (template_id 507) (schema_id 8) (version 5)))
     (round_trip (Ok true)))
    (RetransmitRequest (size 42)
     (header ((block_length 30) (template_id 508) (schema_id 8) (version 5)))
     (round_trip (Ok true)))
    (Retransmission (size 43)
     (header ((block_length 31) (template_id 509) (schema_id 8) (version 5)))
     (round_trip (Ok true)))
    (RetransmitReject (size 87)
     (header ((block_length 75) (template_id 510) (schema_id 8) (version 5)))
     (round_trip (Ok true)))
    (NotApplied (size 29)
     (header ((block_length 17) (template_id 513) (schema_id 8) (version 5)))
     (round_trip (Ok true)))
    |}]
;;

let%expect_test "decode errors" =
  let b = encode negotiate in
  Sbe.set_u16 b ~pos:8 99;
  print_s [%sexp (decode b : Message.t Or_error.t)];
  Sbe.set_u16 b ~pos:8 8;
  Sbe.set_u16 b ~pos:6 999;
  print_s [%sexp (decode b : Message.t Or_error.t)];
  print_s [%sexp (decode (Bigstring.sub b ~pos:0 ~len:40) : Message.t Or_error.t)];
  [%expect
    {|
    (Error "unexpected schema id 99")
    (Error "unknown iLink 3 template 999")
    (Error "SOFH size 90 exceeds the 40 bytes received")
    |}]
;;
