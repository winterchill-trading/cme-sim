(** A market-by-price book of one instrument as an MDP 3.0 consumer keeps it, and the
    inverse operation a publisher needs: the incremental entries that turn one book into
    another.

    Entries address a side by [MDPriceLevel] (1 is the best price). [New] inserts at the
    level and pushes the levels below it down (anything beyond the book depth is dropped),
    [Delete] removes the level and pulls the ones below it up, [Change] replaces it. *)

open! Core
module Price := Exchange_core.Price

module Level : sig
  type t =
    { px : Price.t
    ; size : int
    ; orders : int
    }
  [@@deriving sexp, compare, equal]
end

module Side : sig
  type t =
    | Bid
    | Offer
  [@@deriving sexp, compare, equal, enumerate]
end

(** One incremental change, before it is given a security id and [RptSeq]. *)
module Update : sig
  type t =
    { side : Side.t
    ; level : int
    ; action : Mdp3.Update_action.t (** [New], [Change] or [Delete] *)
    ; px : Price.t
    ; size : int
    ; orders : int
    }
  [@@deriving sexp, compare, equal]

  val to_entry : t -> security_id:int -> rpt_seq:int -> Mdp3.Incremental_book.Entry.t
end

type t [@@deriving sexp_of]

(** Default depth 10, as CME publishes outrights. *)
val create : ?depth:int -> unit -> t

val depth : t -> int

(** Best first. *)
val bids : t -> Level.t list

val offers : t -> Level.t list
val clear : t -> unit

(** Applies one book entry. Entries for implied prices are ignored; a [Book_reset] entry
    clears both sides. Fails on a level outside the book. *)
val apply : t -> Mdp3.Incremental_book.Entry.t -> unit Or_error.t

(** Replaces the book with the bid (['0']) and offer (['1']) entries of a snapshot. *)
val apply_snapshot : t -> Mdp3.Snapshot_full_refresh.t -> unit

(** Updates for one side that turn [old] into [new_], both best first and at most [depth]
    levels: deletions (from the top), then insertions and changes in level order. *)
val diff : Side.t -> old:Level.t list -> new_:Level.t list -> Update.t list
