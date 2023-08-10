let main vocab_file merges_file =
  let text = In_channel.input_all stdin in
  let tokenizer = Tokenizers.Bpe.Tokenizer.load vocab_file merges_file in
  let ids = Tokenizers.Bpe.Tokenizer.encode tokenizer text in
  List.iter (fun x -> Printf.printf "%d\n" x) ids
;;

let command =
  Command.basic
    ~summary:"Tokenize some text"
    (let%map_open.Command vocab_file = anon ("vocab_file" %: string)
     and merges_file = anon ("merges_file" %: string) in
     fun () -> main vocab_file merges_file)
  |> Command_unix.run
;;
