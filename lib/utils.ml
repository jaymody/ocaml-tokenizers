let reverse_hashtbl tbl =
  let t = Hashtbl.create (Hashtbl.length tbl) in
  Hashtbl.iter (fun k v -> Hashtbl.add t v k) tbl;
  t
;;

let compare_in_order comparators a b =
  let rec aux = function
    | [] -> 0
    | f :: fs ->
      let v = f a b in
      if v = 0 then aux fs else v
  in
  aux comparators
;;

let get_pairs =
  let[@tail_mod_cons] rec aux = function
    | a :: (b :: _ as tl) -> (a, b) :: aux tl
    | _ -> []
  in
  aux
;;

let utf8_indices text =
  let len = String.length text in
  let[@tail_mod_cons] rec aux i =
    let j = BatUTF8.next text i in
    if j < len then i :: aux j else [ i; j ]
  in
  aux 0
;;
