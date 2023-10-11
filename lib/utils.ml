let reverse_hashtbl tbl =
  let t = Hashtbl.create (Hashtbl.length tbl) in
  Hashtbl.iter (fun k v -> Hashtbl.add t v k) tbl;
  t
;;
