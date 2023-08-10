# OCaml Tokenizers
Transformer tokenizers in OCaml.

### Usage
Dependencies:
```shell
opam switch create . -w
```

Run CLI:
```shell
printf "This is some text" | dune exec -- bin/main.exe data/encoder.json data/vocab.bpe
```
