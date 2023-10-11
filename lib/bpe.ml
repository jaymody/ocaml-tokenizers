let load_vocab filepath =
  let tbl = Hashtbl.create 50257 in
  Yojson.Basic.from_file filepath
  |> Yojson.Basic.Util.to_assoc
  |> List.iter (fun (k, v) -> Hashtbl.add tbl k (Yojson.Basic.Util.to_int v));
  tbl
;;

let load_merges filepath =
  let tbl = Hashtbl.create 50000 in
  let _ =
    In_channel.open_text filepath
    |> In_channel.input_all
    |> String.trim (* remove the final new-line *)
    |> String.split_on_char '\n'
    |> List.tl (* remove the first line which is the version number *)
    |> List.fold_left
         (fun i line ->
           Hashtbl.add
             tbl
             (match String.split_on_char ' ' line with
              | [ a; b ] -> a, b
              | _ -> invalid_arg (Format.sprintf "Invalid line: %s" line))
             i;
           i + 1)
         0
  in
  tbl
;;

let encode_bytes_to_visible_unicode text =
  String.fold_left
    (fun s c ->
      (match Char.code c with
       | i when i < 33 -> 256 + i
       | i when 126 < i && i < 161 -> 162 + i (* 256 + 33 + (i - 127) *)
       | 173 -> 323 (* 256 + 33 + (161 - 127) *)
       | i -> i)
      |> BatUChar.of_int
      |> BatUTF8.of_char
      |> BatBytes.of_string
      |> BatBytes.cat s)
    BatBytes.empty
    text
  |> BatBytes.to_string
;;

let decode_visible_unicode_to_bytes text =
  BatUTF8.fold
    (fun s c ->
      (match BatUChar.code c with
       | i when 255 < i && i < 289 -> i - 256
       | i when 288 < i && i < 323 -> i - 162
       | 323 -> 173
       | i -> i)
      |> Char.chr
      |> BatBytes.make 1
      |> BatBytes.cat s)
    BatBytes.empty
    text
  |> BatBytes.to_string
;;

let bpe text ranks =
  let module Token = struct
    type t =
      { i : int
      ; j : int
      }

    let create i j = { i; j }
    let compare a b = if a.i = b.i then a.j - b.j else a.i - b.i
    let to_str { i; j } = String.sub text i (j - i)
  end
  in
  let module Pair = struct
    type t =
      { l : Token.t
      ; r : Token.t
      ; score : int
      }

    let create l r =
      match Hashtbl.find_opt ranks (Token.to_str l, Token.to_str r) with
      | None -> None
      | Some score -> Some { l; r; score }
    ;;

    let compare a b = a.score - b.score

    let merge { l; r; _ } : Token.t =
      assert (l.j = r.i);
      { i = l.i; j = r.j }
    ;;
  end
  in
  let module PairPq = Algos.Pq.Make (Pair) in
  let module Tokens = Algos.Avl.Make (Token) in
  let[@warning "-32-27"] print_info tokens pq =
    (* let pair_to_str ({ l; r; score } : Pair.t) =
      "(" ^ Token.to_str l ^ ", " ^ Token.to_str r ^ ", " ^ Int.to_string score ^ ")"
    in
    let tokens_to_str tokens =
      tokens
      |> Tokens.to_list
      |> List.map fst
      |> List.map Token.to_str
      |> List.iter (fun s -> Printf.printf "%s " s);
      print_endline ""
    in
    let pq_to_str pq =
      PairPq.to_list pq
      |> List.map pair_to_str
      |> List.iter (fun s -> Printf.printf "%s " s);
      print_endline ""
    in
    tokens_to_str tokens;
    pq_to_str pq;
    print_endline "-----" *)
    ()
  in
  let update_prev_next (token : Token.t) tokens pq =
    let push_pair_opt pair pq =
      match pair with
      | None -> pq
      | Some pair -> PairPq.push pair pq
    in
    let rec get_prev = function
      | Tokens.Empty -> None
      | Tokens.Node n ->
        if token.i = n.k.j
        then Pair.create n.k token
        else if token.i < n.k.j
        then get_prev n.l
        else get_prev n.r
    in
    let rec get_next = function
      | Tokens.Empty -> None
      | Tokens.Node n ->
        if token.j = n.k.i
        then Pair.create token n.k
        else if token.j < n.k.i
        then get_next n.l
        else get_next n.r
    in
    pq |> push_pair_opt (get_prev tokens) |> push_pair_opt (get_next tokens)
  in
  let merge_pair tokens pq (pair : Pair.t) =
    if Option.is_some (Tokens.find pair.l tokens)
       && Option.is_some (Tokens.find pair.r tokens)
    then (
      let tokens = tokens |> Tokens.pop pair.l |> snd |> Tokens.pop pair.r |> snd in
      let new_token = Pair.merge pair in
      let tokens = Tokens.insert new_token "throwaway" tokens in
      let pq = update_prev_next new_token tokens pq in
      print_info tokens pq;
      tokens, pq)
    else tokens, pq
  in
  let rec aux (tokens, pq) =
    match PairPq.pop pq with
    | None, _ -> tokens
    | Some pair, pq -> aux (merge_pair tokens pq pair)
  in
  let init_tokens =
    let len = String.length text in
    let rec aux i acc =
      let j = BatUTF8.next text i in
      if j < len
      then aux j (Tokens.insert (Token.create i j) "throwaway" acc)
      else Tokens.insert (Token.create i j) "throwaway" acc
    in
    aux 0 Tokens.empty
  in
  let init_pq =
    let rec aux pq = function
      | a :: (b :: _ as tl) ->
        aux
          (match Pair.create a b with
           | None -> pq
           | Some pair -> PairPq.push pair pq)
          tl
      | _ -> pq
    in
    Tokens.to_list init_tokens |> List.map fst |> aux PairPq.empty
  in
  print_info init_tokens init_pq;
  aux (init_tokens, init_pq) |> Tokens.to_list |> List.map fst |> List.map Token.to_str
;;

module Tokenizer : sig
  type t

  val load : string -> string -> t
  val encode : t -> string -> int list
  val decode : t -> int list -> string
end = struct
  type t =
    { stoi : (string, int) Hashtbl.t
    ; itos : (int, string) Hashtbl.t
    ; ranks : (string * string, int) Hashtbl.t
    }

  let load vocab_filepath merges_filepath =
    let stoi = load_vocab vocab_filepath in
    let itos = Utils.reverse_hashtbl stoi in
    let ranks = load_merges merges_filepath in
    { stoi; itos; ranks }
  ;;

  let encode { stoi; ranks; _ } text =
    let text = encode_bytes_to_visible_unicode text in
    let tokens = bpe text ranks in
    let ids = List.map (Hashtbl.find stoi) tokens in
    ids
  ;;

  let decode { itos; _ } ids =
    let tokens = List.map (Hashtbl.find itos) ids in
    let text = String.concat "" tokens in
    let text = decode_visible_unicode_to_bytes text in
    text
  ;;
end
