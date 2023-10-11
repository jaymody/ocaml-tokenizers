# OCaml Tokenizers
Transformer tokenizers in OCaml.

Currently, only BPE "inference" is implemented, but I hope to expand it with training and WordPiece tokenization.

### Usage
Dependencies:
```shell
opam switch create . -w
```

Run CLI (converts stdin text to BPE token ids):
```shell
> printf "This is some text" | dune exec -- bin/main.exe
1212
318
617
2420
```

### Test
To compare the BPE implementation to [`tiktoken`](https://github.com/openai/tiktoken), run:

```shell
cat some_file.txt | python -c "import sys;import tiktoken;print(*tiktoken.get_encoding('gpt2').encode(sys.stdin.read()),sep='\n')"
```

And compare with:

```shell
cat some_file.txt | dune exec -- bin/main.exe
```

### Todo
- [ ] Add ability to download BPE vocab files.
- [ ] Implement training for BPE.
- [ ] Fix issue in BPE where the python version doesn't merge two consecutive new lines (leaves it as [198, 198], i.e. [\n, \n]) while this version merges them (to [ 628 ] i.e. [\n\n])). This is due to the last two lines in the [original implementations regex](https://github.com/openai/gpt-2/blob/a74da5d99abaaba920de8131d64da2862a8f213b/src/encoder.py#L53), `"\s+(?!\S)"` which will always separate two consecutive `\n` if the second `\n` is followed by a non whitespace character. Effectively, this means that BPE tokenization without the regex gives a slightly different result. This is more a bug with the OpenAI implementation (BPE should give the same result with or without regex ideally) but nonetheless should be dealt with since it might effect generation negatively.
- [ ] Implement wordpiece tokenization for BERT, reference: https://github.com/google-research/bert/blob/master/tokenization.py
- [ ] Add example of streaming the output of tokenization to something like `llama.c`.
