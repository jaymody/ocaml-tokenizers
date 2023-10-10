# OCaml Tokenizers
Transformer tokenizers in OCaml.

### Usage
Dependencies:
```shell
opam switch create . -w
```

Run CLI:
```shell
printf "This is some text" | dune exec -- bin/main.exe
```

### Test
Comparing BPE implementation against tiktoken:

```shell
cat some_file.txt | python -c "import sys;import tiktoken;print(*tiktoken.get_encoding('gpt2').encode(sys.stdin.read()),sep='\n')"
```

vs

```shell
cat some_file.txt | dune exec -- bin/main.exe
```
