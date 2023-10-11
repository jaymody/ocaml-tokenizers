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

    (* Note, we not only need to compare score, but also the starting positions.
       Consider the following list of tokens [a; a; a]. If "a a" is a merge,
       then which do we merge first? The first pair of a's, or the second? To be
       consistent tiktoken, we prioritize by start position if there are
       multiple of the same merge pairs. *)
    let compare =
      Utils.compare_in_order
        [ (fun a b -> a.score - b.score)
        ; (fun a b -> Token.compare a.l b.l)
        ; (fun a b -> Token.compare a.r b.r) (* technically this shouldn't be needed *)
        ]
    ;;

    let merge { l; r; _ } : Token.t =
      (* TODO: maybe remove the assert? *)
      assert (l.j = r.i);
      { i = l.i; j = r.j }
    ;;
  end
  in
  let module PairPq = Algos.Pq.Make (Pair) in
  let module Tokens = Algos.Avl.Make (Token) in
  let push_pq_opt pq = Option.fold ~none:pq ~some:(fun p -> PairPq.push p pq) in
  let get_prev_pair (token : Token.t) =
    let rec aux = function
      | Tokens.Empty -> None
      | Tokens.Node n ->
        if token.i = n.k.j
        then Pair.create n.k token
        else if token.i < n.k.j
        then aux n.l
        else aux n.r
    in
    aux
  in
  let get_next_pair (token : Token.t) =
    let rec aux = function
      | Tokens.Empty -> None
      | Tokens.Node n ->
        if token.j = n.k.i
        then Pair.create token n.k
        else if token.j < n.k.i
        then aux n.l
        else aux n.r
    in
    aux
  in
  let rec merge_pairs tokens pq =
    match PairPq.pop pq with
    | None, _ -> tokens
    | Some pair, pq ->
      (match Tokens.find pair.l tokens, Tokens.find pair.r tokens with
       | Some _, Some _ ->
         let tokens = tokens |> Tokens.pop pair.l |> snd |> Tokens.pop pair.r |> snd in
         let merged_token = Pair.merge pair in
         let tokens = Tokens.insert merged_token "throwaway" tokens in
         let pq = push_pq_opt pq (get_prev_pair merged_token tokens) in
         let pq = push_pq_opt pq (get_next_pair merged_token tokens) in
         merge_pairs tokens pq
       | _ -> merge_pairs tokens pq)
  in
  let tokens_list =
    List.map (fun (i, j) -> Token.create i j) (Utils.utf8_indices text |> Utils.get_pairs)
  in
  merge_pairs
    (List.fold_left
       (fun tokens t -> Tokens.insert t "throwaway" tokens)
       Tokens.empty
       tokens_list)
    (List.fold_left
       (fun pq (l, r) -> push_pq_opt pq (Pair.create l r))
       PairPq.empty
       (Utils.get_pairs tokens_list))
  |> Tokens.to_list
  |> List.map fst
  |> List.map Token.to_str
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
