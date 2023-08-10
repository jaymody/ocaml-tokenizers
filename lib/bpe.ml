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
  BatUTF8.map
    (fun c ->
      BatUChar.of_int
        (match BatUChar.int_of c with
         | i when i < 33 -> 256 + i
         | i when 126 < i && i < 161 -> 162 + i (* 256 + 33 + (i - 127) *)
         | 173 -> 323 (* 256 + 33 + (161 - 127) *)
         | i -> i))
    text
;;

let decode_visible_unicode_to_bytes text =
  BatUTF8.map
    (fun c ->
      BatUChar.of_int
        (match BatUChar.int_of c with
         | i when 255 < i && i < 289 -> i - 256
         | i when 288 < i && i < 323 -> i - 162
         | 323 -> 173
         | i -> i))
    text
;;

let get_min_pair tokens ranks =
  let rec aux min_pair = function
    | a :: (b :: _ as tl) ->
      let new_min_pair =
        match min_pair with
        | None -> if Hashtbl.mem ranks (a, b) then Some (a, b) else None
        | Some p ->
          (match Hashtbl.find_opt ranks (a, b) with
           | Some r when r < Hashtbl.find ranks p -> Some (a, b)
           | _ -> Some p)
      in
      aux new_min_pair tl
    | _ -> min_pair
  in
  aux None tokens
;;

let merge_tokens tokens pair =
  let rec aux acc = function
    | a :: b :: tl when (a, b) = pair -> aux ((a ^ b) :: acc) tl
    | a :: tl -> aux (a :: acc) tl
    | _ -> acc
  in
  List.rev (aux [] tokens)
;;

let bpe text ranks =
  let rec aux tokens =
    match get_min_pair tokens ranks with
    | None -> tokens
    | Some pair -> aux (merge_tokens tokens pair)
  in
  Utils.str_to_utf8_list text |> aux
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
