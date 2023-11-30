let main (vocab_file, merges_file) =
  let text = In_channel.input_all stdin in
  let tokenizer = Tokenizers.Bpe.load vocab_file merges_file in
  let ids = Tokenizers.Bpe.encode tokenizer text in
  let reconstructed_text = Tokenizers.Bpe.decode tokenizer ids in
  assert (reconstructed_text = text);
  List.iter (fun x -> Printf.printf "%d\n" x) ids
;;

let args =
  let vocab_file = ref "data/encoder.json" in
  let merges_file = ref "data/vocab.bpe" in
  let open Stdlib in
  Arg.parse
    [ "--vocab_file", Arg.Set_string vocab_file, "Path to vocab file (encoder.json)"
    ; "--merges_file", Arg.Set_string merges_file, "Path to bpe merges file (vocab.bpe)"
    ]
    (fun s -> Printf.printf "%s" s)
    "main.exe [vocab_file] [merges_file]";
  !vocab_file, !merges_file
;;

main args
