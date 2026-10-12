(** iLink 3 order entry messages (schema id 8, version 5): the FIXP session layer and the
    in-scope application messages.

    A frame on the wire is a 4-byte SOFH, the 8-byte SBE message header and the root
    block, followed by repeating groups or variable-length data where the template has
    them. Encoders write every field of the root block at the offsets of the published
    schema, fields this codec does not model are written as their null value. Decoders
    read the root block by offset and use the header's [BlockLength] to find what follows
    it, so newer schema versions that append fields still decode. *)

open! Core
module Price := Exchange_core.Price

val schema_id : int
val schema_version : int

module Side : sig
  type t =
    | Buy
    | Sell
  [@@deriving sexp, compare, equal, enumerate]

  val to_int : t -> int
  val of_int : int -> t option
end

module Ord_type : sig
  type t =
    | Market_with_protection (** ['1'] *)
    | Limit (** ['2'] *)
    | Stop_with_protection (** ['3'] *)
    | Stop_limit (** ['4'] *)
    | Market_with_leftover_as_limit (** ['K'] *)
  [@@deriving sexp, compare, equal, enumerate]

  val to_char : t -> char
  val of_char : char -> t option
end

module Time_in_force : sig
  type t =
    | Day (** 0 *)
    | Gtc (** 1 *)
    | Fak (** 3, fill and kill *)
    | Fok (** 4, fill or kill *)
    | Gtd (** 6 *)
  [@@deriving sexp, compare, equal, enumerate]

  val to_int : t -> int
  val of_int : int -> t option
end

(** {2 Session layer (FIXP)} *)

module Negotiate : sig
  type t =
    { hmac_signature : string (** 32 bytes *)
    ; access_key_id : string (** 20 bytes *)
    ; uuid : int
    ; request_timestamp : int
    ; session : string (** 3 bytes *)
    ; firm : string (** 5 bytes *)
    }
  [@@deriving sexp, compare, equal]
end

module Negotiation_response : sig
  type t =
    { uuid : int
    ; request_timestamp : int
    ; secret_key_secure_id_expiration : int option
    ; previous_seq_no : int
    ; previous_uuid : int
    }
  [@@deriving sexp, compare, equal]
end

module Negotiation_reject : sig
  type t =
    { reason : string
    ; uuid : int
    ; request_timestamp : int
    ; error_codes : int
    }
  [@@deriving sexp, compare, equal]
end

module Establish : sig
  type t =
    { hmac_signature : string
    ; access_key_id : string
    ; trading_system_name : string
    ; trading_system_version : string
    ; trading_system_vendor : string
    ; uuid : int
    ; request_timestamp : int
    ; next_seq_no : int
    ; session : string
    ; firm : string
    ; keep_alive_interval : int (** milliseconds *)
    }
  [@@deriving sexp, compare, equal]
end

module Establishment_ack : sig
  type t =
    { uuid : int
    ; request_timestamp : int
    ; next_seq_no : int
    ; previous_seq_no : int
    ; previous_uuid : int
    ; keep_alive_interval : int
    ; secret_key_secure_id_expiration : int option
    }
  [@@deriving sexp, compare, equal]
end

module Establishment_reject : sig
  type t =
    { reason : string
    ; uuid : int
    ; request_timestamp : int
    ; next_seq_no : int
    ; error_codes : int
    }
  [@@deriving sexp, compare, equal]
end

module Sequence : sig
  type t =
    { uuid : int
    ; next_seq_no : int
    ; keep_alive_interval_lapsed : bool
    }
  [@@deriving sexp, compare, equal]
end

module Terminate : sig
  type t =
    { reason : string
    ; uuid : int
    ; request_timestamp : int
    ; error_codes : int
    }
  [@@deriving sexp, compare, equal]
end

module Retransmit_request : sig
  type t =
    { uuid : int
    ; last_uuid : int option
    ; request_timestamp : int
    ; from_seq_no : int
    ; msg_count : int
    }
  [@@deriving sexp, compare, equal]
end

module Retransmission : sig
  type t =
    { uuid : int
    ; last_uuid : int option
    ; request_timestamp : int
    ; from_seq_no : int
    ; msg_count : int
    }
  [@@deriving sexp, compare, equal]
end

module Retransmit_reject : sig
  type t =
    { reason : string
    ; uuid : int
    ; last_uuid : int option
    ; request_timestamp : int
    ; error_codes : int
    }
  [@@deriving sexp, compare, equal]
end

module Not_applied : sig
  type t =
    { uuid : int
    ; from_seq_no : int
    ; msg_count : int
    }
  [@@deriving sexp, compare, equal]
end

(** {2 Messages} *)

module Message : sig
  type t =
    | Negotiate of Negotiate.t
    | Negotiation_response of Negotiation_response.t
    | Negotiation_reject of Negotiation_reject.t
    | Establish of Establish.t
    | Establishment_ack of Establishment_ack.t
    | Establishment_reject of Establishment_reject.t
    | Sequence of Sequence.t
    | Terminate of Terminate.t
    | Retransmit_request of Retransmit_request.t
    | Retransmission of Retransmission.t
    | Retransmit_reject of Retransmit_reject.t
    | Not_applied of Not_applied.t
  [@@deriving sexp, compare, equal]

  val template_id : t -> int

  (** Template name without the id, e.g. ["NewOrderSingle"]. *)
  val name : t -> string
end

(** The whole frame: SOFH, header, body. *)
val encode : Message.t -> Bigstring.t

(** Decodes one whole frame (as produced by {!encode}). Unknown templates and a wrong
    schema id are errors. *)
val decode : Bigstring.t -> Message.t Or_error.t
