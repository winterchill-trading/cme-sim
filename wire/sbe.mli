(** Simple Binary Encoding primitives shared by iLink 3 and MDP 3.0: little-endian field
    access at fixed offsets, CME null values, [PRICE9] prices, the 8-byte message header
    and the 4-byte Simple Open Framing Header (SOFH).

    Every accessor takes the absolute position in the buffer; codecs add the offset of the
    root block or group entry themselves. *)

open! Core
module Price := Exchange_core.Price

(** {2 Integers} *)

val set_u8 : Bigstring.t -> pos:int -> int -> unit
val get_u8 : Bigstring.t -> pos:int -> int
val set_i8 : Bigstring.t -> pos:int -> int -> unit
val get_i8 : Bigstring.t -> pos:int -> int
val set_u16 : Bigstring.t -> pos:int -> int -> unit
val get_u16 : Bigstring.t -> pos:int -> int
val set_i16 : Bigstring.t -> pos:int -> int -> unit
val get_i16 : Bigstring.t -> pos:int -> int
val set_u32 : Bigstring.t -> pos:int -> int -> unit
val get_u32 : Bigstring.t -> pos:int -> int
val set_i32 : Bigstring.t -> pos:int -> int -> unit
val get_i32 : Bigstring.t -> pos:int -> int

(** [uInt64] values that fit an OCaml [int] (ids, nanosecond timestamps). *)
val set_u64 : Bigstring.t -> pos:int -> int -> unit

val get_u64 : Bigstring.t -> pos:int -> int
val set_i64 : Bigstring.t -> pos:int -> Int64.t -> unit
val get_i64 : Bigstring.t -> pos:int -> Int64.t

(** {2 Optional integers}

    [None] is written as the type's null value: 255, 65535, 4294967295, [INT32_MAX] for
    MDP [Int32NULL], all ones for [uInt64NULL]. *)

val set_u8_opt : Bigstring.t -> pos:int -> int option -> unit
val get_u8_opt : Bigstring.t -> pos:int -> int option
val set_u16_opt : Bigstring.t -> pos:int -> int option -> unit
val get_u16_opt : Bigstring.t -> pos:int -> int option
val set_u32_opt : Bigstring.t -> pos:int -> int option -> unit
val get_u32_opt : Bigstring.t -> pos:int -> int option
val set_i32_opt : Bigstring.t -> pos:int -> int option -> unit
val get_i32_opt : Bigstring.t -> pos:int -> int option
val set_u64_opt : Bigstring.t -> pos:int -> int option -> unit
val get_u64_opt : Bigstring.t -> pos:int -> int option

(** {2 Characters and fixed-length strings} *)

val set_char : Bigstring.t -> pos:int -> char -> unit
val get_char : Bigstring.t -> pos:int -> char

(** [charNULL]: [None] is byte 0. *)
val set_char_opt : Bigstring.t -> pos:int -> char option -> unit

val get_char_opt : Bigstring.t -> pos:int -> char option

(** Writes [s] into a [len]-byte field padded with NUL bytes. Raises if [s] is longer. *)
val set_string : Bigstring.t -> pos:int -> len:int -> string -> unit

(** Reads a [len]-byte field up to the first NUL byte. *)
val get_string : Bigstring.t -> pos:int -> len:int -> string

(** {2 Booleans} *)

(** [BooleanFlag]: 0 or 1. *)
val set_bool : Bigstring.t -> pos:int -> bool -> unit

val get_bool : Bigstring.t -> pos:int -> bool

(** [BooleanNULL]: 0, 1 or 255. *)
val set_bool_opt : Bigstring.t -> pos:int -> bool option -> unit

val get_bool_opt : Bigstring.t -> pos:int -> bool option

(** {2 Prices}

    [PRICE9] is an [int64] mantissa with exponent -9; [PRICENULL9] uses [INT64_MAX] as
    null. Prices with more than 8 decimals are truncated on decode. *)

val price_to_mantissa : Price.t -> Int64.t
val price_of_mantissa : Int64.t -> Price.t
val set_price : Bigstring.t -> pos:int -> Price.t -> unit
val get_price : Bigstring.t -> pos:int -> Price.t
val set_price_opt : Bigstring.t -> pos:int -> Price.t option -> unit
val get_price_opt : Bigstring.t -> pos:int -> Price.t option

(** {2 Message header} *)

module Header : sig
  type t =
    { block_length : int
    ; template_id : int
    ; schema_id : int
    ; version : int
    }
  [@@deriving sexp, compare, equal]

  val size : int
  val write : Bigstring.t -> pos:int -> t -> unit
  val read : Bigstring.t -> pos:int -> t
end

(** {2 Repeating groups} *)

module Group : sig
  (** [groupSize]: [blockLength u16] and [numInGroup u8], 3 bytes. *)
  val size : int

  (** [groupSize8Byte]: [blockLength u16], [numInGroup u8] at offset 7, 8 bytes. *)
  val size_8byte : int

  (** Writes a group header; returns the position of the first entry. *)
  val write : ?eight_byte:bool -> Bigstring.t -> pos:int -> block_length:int -> int -> int

  (** Reads a group header: [(block_length, num_in_group, first_entry_pos)]. *)
  val read : ?eight_byte:bool -> Bigstring.t -> pos:int -> int * int * int
end

(** {2 Simple Open Framing Header} *)

module Sofh : sig
  (** [MessageSize u16] (the whole message including these 4 bytes) and
      [EncodingType u16]. *)
  val size : int

  (** [0xCAFE]: CME SBE version 1.0, little endian. *)
  val encoding_type : int

  val write : Bigstring.t -> pos:int -> message_size:int -> unit

  (** The message size; fails on a wrong encoding type or a size below 4. *)
  val read : Bigstring.t -> pos:int -> int Or_error.t
end

(** Lower-case hex dump, 16 bytes per line, for tests and debugging. *)
val hex : Bigstring.t -> string
