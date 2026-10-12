open! Core
module Price = Exchange_core.Price

let set_u8 b ~pos v = Bigstring.set_uint8_exn b ~pos v
let get_u8 b ~pos = Bigstring.get_uint8 b ~pos
let set_i8 b ~pos v = Bigstring.set_int8_exn b ~pos v
let get_i8 b ~pos = Bigstring.get_int8 b ~pos
let set_u16 b ~pos v = Bigstring.set_uint16_le_exn b ~pos v
let get_u16 b ~pos = Bigstring.get_uint16_le b ~pos
let set_i16 b ~pos v = Bigstring.set_int16_le_exn b ~pos v
let get_i16 b ~pos = Bigstring.get_int16_le b ~pos
let set_u32 b ~pos v = Bigstring.set_uint32_le_exn b ~pos v
let get_u32 b ~pos = Bigstring.get_uint32_le b ~pos
let set_i32 b ~pos v = Bigstring.set_int32_le_exn b ~pos v
let get_i32 b ~pos = Bigstring.get_int32_le b ~pos
let set_i64 b ~pos v = Bigstring.set_int64_t_le b ~pos v
let get_i64 b ~pos = Bigstring.get_int64_t_le b ~pos
let set_u64 b ~pos v = set_i64 b ~pos (Int64.of_int v)
let get_u64 b ~pos = Int64.to_int_exn (get_i64 b ~pos)
let u8_null = 255
let u16_null = 65535
let u32_null = 4294967295
let i32_null = Int32.to_int_exn Int32.max_value
let u64_null = -1L
let set_opt set ~null b ~pos v = set b ~pos (Option.value v ~default:null)

let get_opt get ~null b ~pos =
  let v = get b ~pos in
  if v = null then None else Some v
;;

let set_u8_opt = set_opt set_u8 ~null:u8_null
let get_u8_opt = get_opt get_u8 ~null:u8_null
let set_u16_opt = set_opt set_u16 ~null:u16_null
let get_u16_opt = get_opt get_u16 ~null:u16_null
let set_u32_opt = set_opt set_u32 ~null:u32_null
let get_u32_opt = get_opt get_u32 ~null:u32_null
let set_i32_opt = set_opt set_i32 ~null:i32_null
let get_i32_opt = get_opt get_i32 ~null:i32_null

let set_u64_opt b ~pos = function
  | None -> set_i64 b ~pos u64_null
  | Some v -> set_u64 b ~pos v
;;

let get_u64_opt b ~pos =
  let v = get_i64 b ~pos in
  if Int64.equal v u64_null then None else Some (Int64.to_int_exn v)
;;

let set_char b ~pos c = Bigstring.set b pos c
let get_char b ~pos = Bigstring.get b pos
let set_char_opt b ~pos c = set_char b ~pos (Option.value c ~default:'\000')

let get_char_opt b ~pos =
  match get_char b ~pos with
  | '\000' -> None
  | c -> Some c
;;

let set_string b ~pos ~len s =
  if String.length s > len
  then raise_s [%message "Sbe.set_string: value too long" (s : string) (len : int)];
  Bigstring.From_string.blit ~src:s ~src_pos:0 ~len:(String.length s) ~dst:b ~dst_pos:pos;
  for i = String.length s to len - 1 do
    Bigstring.set b (pos + i) '\000'
  done
;;

let get_string b ~pos ~len =
  let s = Bigstring.To_string.sub b ~pos ~len in
  match String.index s '\000' with
  | None -> s
  | Some i -> String.prefix s i
;;

let set_bool b ~pos v = set_u8 b ~pos (Bool.to_int v)
let get_bool b ~pos = get_u8 b ~pos <> 0
let set_bool_opt b ~pos v = set_u8_opt b ~pos (Option.map v ~f:Bool.to_int)
let get_bool_opt b ~pos = Option.map (get_u8_opt b ~pos) ~f:(fun v -> v <> 0)

(* [Price] counts units of 10^-8; PRICE9 mantissas count 10^-9. *)
let price_to_mantissa p = Int64.(of_int (Price.to_units p) * 10L)
let price_of_mantissa m = Price.of_units (Int64.to_int_exn Int64.(m / 10L))
let price_null = Int64.max_value
let set_price b ~pos p = set_i64 b ~pos (price_to_mantissa p)
let get_price b ~pos = price_of_mantissa (get_i64 b ~pos)

let set_price_opt b ~pos = function
  | None -> set_i64 b ~pos price_null
  | Some p -> set_price b ~pos p
;;

let get_price_opt b ~pos =
  let m = get_i64 b ~pos in
  if Int64.equal m price_null then None else Some (price_of_mantissa m)
;;

module Header = struct
  type t =
    { block_length : int
    ; template_id : int
    ; schema_id : int
    ; version : int
    }
  [@@deriving sexp, compare, equal]

  let size = 8

  let write b ~pos t =
    set_u16 b ~pos t.block_length;
    set_u16 b ~pos:(pos + 2) t.template_id;
    set_u16 b ~pos:(pos + 4) t.schema_id;
    set_u16 b ~pos:(pos + 6) t.version
  ;;

  let read b ~pos =
    { block_length = get_u16 b ~pos
    ; template_id = get_u16 b ~pos:(pos + 2)
    ; schema_id = get_u16 b ~pos:(pos + 4)
    ; version = get_u16 b ~pos:(pos + 6)
    }
  ;;
end

module Group = struct
  let size = 3
  let size_8byte = 8

  let write ?(eight_byte = false) b ~pos ~block_length n =
    set_u16 b ~pos block_length;
    if eight_byte
    then (
      for i = 2 to 6 do
        set_u8 b ~pos:(pos + i) 0
      done;
      set_u8 b ~pos:(pos + 7) n;
      pos + size_8byte)
    else (
      set_u8 b ~pos:(pos + 2) n;
      pos + size)
  ;;

  let read ?(eight_byte = false) b ~pos =
    let block_length = get_u16 b ~pos in
    if eight_byte
    then block_length, get_u8 b ~pos:(pos + 7), pos + size_8byte
    else block_length, get_u8 b ~pos:(pos + 2), pos + size
  ;;
end

module Sofh = struct
  let size = 4
  let encoding_type = 0xCAFE

  let write b ~pos ~message_size =
    set_u16 b ~pos message_size;
    set_u16 b ~pos:(pos + 2) encoding_type
  ;;

  let read b ~pos =
    let message_size = get_u16 b ~pos in
    let enc = get_u16 b ~pos:(pos + 2) in
    if enc <> encoding_type
    then Or_error.error_s [%message "unexpected SOFH encoding type" (enc : int)]
    else if message_size < size
    then Or_error.error_s [%message "SOFH message size too small" (message_size : int)]
    else Ok message_size
  ;;
end

let hex b =
  let n = Bigstring.length b in
  let buf = Buffer.create (n * 3) in
  for i = 0 to n - 1 do
    if i > 0 then Buffer.add_char buf (if i % 16 = 0 then '\n' else ' ');
    Buffer.add_string buf (sprintf "%02x" (Char.to_int (Bigstring.get b i)))
  done;
  Buffer.contents buf
;;
