open! Core

val reverse_hashtbl
  :  (module Hashtbl.Key with type t = 'v)
  -> ('k, 'v) Base.Hashtbl.t
  -> ('v, 'k) Base.Hashtbl.t

val get_pairs : 'a list -> ('a * 'a) list

(* TODO: dedup these types by minting [utils_intf.ml] *)
module type Elt = sig
  type t

  val compare : t -> t -> int
end

module Make_pq : (Elt : Elt) -> sig
  type t

  val push : t -> Elt.t -> t
  val pop : t -> Elt.t option * t
  val of_list : Elt.t list -> t
end

module Make_avl : (Elt : Elt) -> sig
  type t = (Elt.t, unit) Base.Avltree.t

  val mem : t -> Elt.t -> bool
  val pop : t -> Elt.t -> t
  val add : t -> Elt.t -> t
  val to_list : t -> Elt.t list
  val of_list : Elt.t list -> t
end
