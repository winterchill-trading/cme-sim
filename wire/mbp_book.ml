open! Core
module Price = Exchange_core.Price

module Level = struct
  type t =
    { px : Price.t
    ; size : int
    ; orders : int
    }
  [@@deriving sexp, compare, equal]
end

module Side = struct
  type t =
    | Bid
    | Offer
  [@@deriving sexp, compare, equal, enumerate]
end

module Update = struct
  type t =
    { side : Side.t
    ; level : int
    ; action : Mdp3.Update_action.t
    ; px : Price.t
    ; size : int
    ; orders : int
    }
  [@@deriving sexp, compare, equal]

  let to_entry t ~security_id ~rpt_seq : Mdp3.Incremental_book.Entry.t =
    let deleted =
      match t.action with
      | Delete | Delete_thru | Delete_from -> true
      | New | Change | Overlay -> false
    in
    { px = Some t.px
    ; size = (if deleted then None else Some t.size)
    ; security_id
    ; rpt_seq
    ; number_of_orders = (if deleted then None else Some t.orders)
    ; price_level = t.level
    ; update_action = t.action
    ; entry_type =
        (match t.side with
         | Bid -> Bid
         | Offer -> Offer)
    }
  ;;
end

type t =
  { depth : int
  ; mutable bids : Level.t list
  ; mutable offers : Level.t list
  }
[@@deriving sexp_of]

let create ?(depth = 10) () = { depth; bids = []; offers = [] }
let depth t = t.depth
let bids t = t.bids
let offers t = t.offers

let clear t =
  t.bids <- [];
  t.offers <- []
;;

let insert_at l i x = List.take l i @ (x :: List.drop l i)
let remove_at l i = List.filteri l ~f:(fun j _ -> j <> i)
let replace_at l i x = List.mapi l ~f:(fun j y -> if j = i then x else y)

(* Applies one update to a side; [i] is the zero-based level. *)
let apply_side ~depth levels (action : Mdp3.Update_action.t) i (level : Level.t) =
  let n = List.length levels in
  match action with
  | New when i <= n && i < depth -> Ok (List.take (insert_at levels i level) depth)
  | (Change | Overlay) when i < n -> Ok (replace_at levels i level)
  | Delete when i < n -> Ok (remove_at levels i)
  | Delete_thru -> Ok []
  | Delete_from -> Ok (List.take levels i)
  | New | Change | Overlay | Delete ->
    Or_error.error_s
      [%message
        "book entry outside the book"
          (action : Mdp3.Update_action.t)
          ~level:(i + 1 : int)
          ~levels:(n : int)]
;;

let apply t (e : Mdp3.Incremental_book.Entry.t) =
  let level () : Level.t =
    { px = Option.value e.px ~default:Price.zero
    ; size = Option.value e.size ~default:0
    ; orders = Option.value e.number_of_orders ~default:0
    }
  in
  let i = e.price_level - 1 in
  match e.entry_type with
  | Implied_bid | Implied_offer -> Ok ()
  | Book_reset ->
    clear t;
    Ok ()
  | Bid ->
    let%map.Or_error bids =
      apply_side ~depth:t.depth t.bids e.update_action i (level ())
    in
    t.bids <- bids
  | Offer ->
    let%map.Or_error offers =
      apply_side ~depth:t.depth t.offers e.update_action i (level ())
    in
    t.offers <- offers
;;

let apply_snapshot t (s : Mdp3.Snapshot_full_refresh.t) =
  let side c =
    List.filter_map s.entries ~f:(fun (e : Mdp3.Snapshot_full_refresh.Entry.t) ->
      match e.px, e.price_level with
      | Some px, Some level when Char.equal e.entry_type c ->
        Some
          ( level
          , { Level.px
            ; size = Option.value e.size ~default:0
            ; orders = Option.value e.number_of_orders ~default:0
            } )
      | _ -> None)
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    |> List.map ~f:snd
    |> fun l -> List.take l t.depth
  in
  t.bids <- side '0';
  t.offers <- side '1'
;;

let diff side ~(old : Level.t list) ~(new_ : Level.t list) =
  let in_list l (x : Level.t) =
    List.exists l ~f:(fun (y : Level.t) -> Price.equal x.px y.px)
  in
  let update action level (x : Level.t) : Update.t =
    { side; level; action; px = x.px; size = x.size; orders = x.orders }
  in
  (* Deletions, each at its current level in the book being transformed. *)
  let current, deletes =
    List.fold old ~init:(old, []) ~f:(fun (current, acc) x ->
      if in_list new_ x
      then current, acc
      else (
        let i, _ =
          List.findi_exn current ~f:(fun _ (y : Level.t) -> Price.equal x.px y.px)
        in
        remove_at current i, update Delete (i + 1) x :: acc))
  in
  (* [current] is now a sorted subsequence of [new_]: walk [new_] in order. *)
  let _, rest =
    List.foldi new_ ~init:(current, []) ~f:(fun i (current, acc) x ->
      match List.nth current i with
      | Some y when Price.equal x.px y.px ->
        if Level.equal x y then current, acc else current, update Change (i + 1) x :: acc
      | _ -> insert_at current i x, update New (i + 1) x :: acc)
  in
  List.rev deletes @ List.rev rest
;;
