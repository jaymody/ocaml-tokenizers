let reverse_hashtbl tbl =
  let t = Hashtbl.create (Hashtbl.length tbl) in
  Hashtbl.iter (fun k v -> Hashtbl.add t v k) tbl;
  t
;;

let str_to_utf8_list s = List.rev (BatUTF8.fold (fun l c -> BatUTF8.of_char c :: l) [] s)
