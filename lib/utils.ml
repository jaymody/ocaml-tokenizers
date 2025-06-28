open! Core

let reverse_hashtbl
      (type v)
      (module V : Hashtbl.Key with type t = v)
      (tbl : (_, v) Hashtbl.t)
  =
  tbl |> Hashtbl.to_alist |> List.map ~f:Tuple2.swap |> Hashtbl.of_alist_exn (module V)
;;

let get_pairs =
  let[@tail_mod_cons] rec aux = function
    | a :: (b :: _ as tl) -> (a, b) :: aux tl
    | _ -> []
  in
  aux
;;

module type Elt = sig
  type t

  val compare : t -> t -> int
end

module Make_pq (Elt : Elt) : sig
  type t

  val push : t -> Elt.t -> t
  val pop : t -> Elt.t option * t
  val of_list : Elt.t list -> t
end = struct
  type t = (Elt.t, unit) Avltree.t

  let compare = [%compare: Elt.t]
  let empty = Avltree.empty

  let pop t =
    match Avltree.first t with
    | None -> None, t
    | Some (first, ()) -> Some first, Avltree.remove t first ~removed:(ref false) ~compare
  ;;

  let push t x = Avltree.add t ~key:x ~data:() ~replace:false ~added:(ref false) ~compare
  let of_list = List.fold ~init:empty ~f:push
end

module Make_avl (Elt : Elt) : sig
  type t = (Elt.t, unit) Avltree.t

  val mem : t -> Elt.t -> bool
  val pop : t -> Elt.t -> t
  val add : t -> Elt.t -> t
  val to_list : t -> Elt.t list
  val of_list : Elt.t list -> t
end = struct
  type t = (Elt.t, unit) Avltree.t

  let compare = [%compare: Elt.t]
  let empty = Avltree.empty
  let mem = Avltree.mem ~compare
  let pop = Avltree.remove ~removed:(ref false) ~compare
  let add t key = Avltree.add t ~key ~data:() ~replace:false ~added:(ref false) ~compare

  let to_list t =
    t |> Avltree.fold ~init:[] ~f:(fun ~key ~data:() acc -> key :: acc) |> List.rev
  ;;

  let of_list = List.fold ~init:empty ~f:add
end
