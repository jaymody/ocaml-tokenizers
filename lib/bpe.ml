open! Core

let load_vocab filepath =
  Yojson.Basic.from_file filepath
  |> Yojson.Basic.Util.to_assoc
  |> List.map ~f:(fun (k, v) -> String.Utf8.of_string k, Yojson.Basic.Util.to_int v)
  |> String.Utf8.Table.of_alist_exn
;;

let load_merges filepath =
  let module Pair = struct
    module T = struct
      type t = String.Utf8.t * String.Utf8.t [@@deriving sexp, compare, equal, hash]
    end

    include Hashable.Make (T)
  end
  in
  In_channel.read_all filepath
  |> String.strip (* remove the final new-line *)
  |> String.split ~on:'\n'
  |> List.tl_exn (* remove the first line which is the version number *)
  |> List.mapi ~f:(fun i line ->
    let line = String.Utf8.of_string line in
    let pair =
      match String.Utf8.split ~on:(Uchar.of_char ' ') line with
      | [ a; b ] -> a, b
      | _ -> raise_s [%message "failed to parse line" (line : String.Utf8.t)]
    in
    pair, i)
  |> Pair.Table.of_alist_exn
;;

let encode_string_to_visible_unicode text =
  let map_to_visible = function
    | i when i < 33 -> 256 + i
    | i when 126 < i && i < 161 -> 162 + i (* 256 + 33 + (i - 127) *)
    | 173 -> 323 (* 256 + 33 + (161 - 127) *)
    | i -> i
  in
  text
  |> String.to_list
  |> List.map ~f:(fun c -> c |> Char.to_int |> map_to_visible |> Uchar.of_scalar_exn)
  |> String.Utf8.of_list
;;

let decode_visible_unicode_to_string text =
  let map_from_visible = function
    | i when 255 < i && i < 289 -> i - 256
    | i when 288 < i && i < 323 -> i - 162
    | 323 -> 173
    | i -> i
  in
  text
  |> String.Utf8.to_list
  |> List.map ~f:(fun (c : Uchar.t) ->
    c |> Uchar.to_scalar |> map_from_visible |> Char.of_int_exn)
  |> String.of_list
;;

let bpe (text : String.Utf8.t) ranks =
  let text = String.Utf8.to_array text in
  let module Token = struct
    type t =
      { i : int
      ; j : int
      }
    [@@deriving compare]

    let create i j = { i; j }
    let compare a b = if a.i = b.i then a.j - b.j else a.i - b.i
    let to_str { i; j } = text |> Array.sub ~pos:i ~len:(j - i) |> String.Utf8.of_array
  end
  in
  let module Pair = struct
    (* Note, the ordering of the fields determines the ordering for compare.

       We not only need to compare score, but also the starting positions.
       Consider the following list of tokens [a; a; a]. If "a a" is a merge,
       then which do we merge first? The first pair of a's, or the second? To be
       consistent tiktoken, we prioritize by start position if there are
       multiple of the same merge pairs.

       Technically, it should be sufficient to only compare score and left,
       since pairs should not overlap. *)
    type t =
      { score : int
      ; l : Token.t
      ; r : Token.t
      }
    [@@deriving compare]

    let create l r =
      match Hashtbl.find ranks (Token.to_str l, Token.to_str r) with
      | None -> None
      | Some score -> Some { l; r; score }
    ;;

    let merge { l; r; _ } : Token.t =
      (* TODO: maybe remove the assert? *)
      assert (l.j = r.i);
      { i = l.i; j = r.j }
    ;;
  end
  in
  let module Tokens = Utils.Make_avl (Token) in
  let module PairPq = Utils.Make_pq (Pair) in
  let push_pq_opt pq = Option.value_map ~default:pq ~f:(fun p -> PairPq.push pq p) in
  let get_prev_pair (token : Token.t) =
    let rec aux = function
      | Avltree.Empty -> None
      | Leaf { key; _ } -> if token.i = key.Token.j then Pair.create key token else None
      | Node n ->
        if token.i = n.key.j
        then Pair.create n.key token
        else if token.i < n.key.j
        then aux n.left
        else aux n.right
    in
    aux
  in
  let get_next_pair (token : Token.t) =
    let rec aux = function
      | Avltree.Empty -> None
      | Leaf { key; _ } -> if token.j = key.Token.i then Pair.create token key else None
      | Node n ->
        if token.j = n.key.i
        then Pair.create token n.key
        else if token.j < n.key.i
        then aux n.left
        else aux n.right
    in
    aux
  in
  let rec merge_pairs tokens pq =
    match PairPq.pop pq with
    | None, _ -> tokens
    | Some pair, pq ->
      (match Tokens.mem tokens pair.l, Tokens.mem tokens pair.r with
       | true, true ->
         let tokens =
           tokens |> fun t -> Tokens.pop t pair.l |> fun t -> Tokens.pop t pair.r
         in
         let merged_token = Pair.merge pair in
         let tokens = Tokens.add tokens merged_token in
         let pq = push_pq_opt pq (get_prev_pair merged_token tokens) in
         let pq = push_pq_opt pq (get_next_pair merged_token tokens) in
         merge_pairs tokens pq
       | _ -> merge_pairs tokens pq)
  in
  let initial_tokens =
    List.init (Array.length text) ~f:(fun i -> Token.create i (i + 1))
  in
  let tokens = Tokens.of_list initial_tokens in
  let pq =
    initial_tokens
    |> Utils.get_pairs
    |> List.filter_map ~f:(fun (l, r) -> Pair.create l r)
    |> PairPq.of_list
  in
  merge_pairs tokens pq |> Tokens.to_list |> List.map ~f:Token.to_str
;;

type t =
  { stoi : (String.Utf8.t, int) Hashtbl.t
  ; itos : (int, String.Utf8.t) Hashtbl.t
  ; ranks : (String.Utf8.t * String.Utf8.t, int) Hashtbl.t
  }

let load vocab_filepath merges_filepath =
  let stoi = load_vocab vocab_filepath in
  let itos = Utils.reverse_hashtbl (module Int) stoi in
  let ranks = load_merges merges_filepath in
  { stoi; itos; ranks }
;;

let encode { stoi; ranks; _ } text =
  let text = encode_string_to_visible_unicode text in
  let tokens = bpe text ranks in
  let ids = List.map tokens ~f:(Hashtbl.find_exn stoi) in
  ids
;;

let decode { itos; _ } ids =
  let tokens = List.map ids ~f:(Hashtbl.find_exn itos) in
  let text = String.Utf8.concat tokens in
  let text = decode_visible_unicode_to_string text in
  text
;;
