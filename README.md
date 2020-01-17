### Environment

```
ocamlbuild --version
ocamlbuild 0.12.0
```
```
ocaml --version
The OCaml toplevel, version 4.05.0
```

### Compilation

```
ocamlbuild -use-menhir main.native
```

### Run the interpreter

```
./main.native [ -h | -eval | -tc | -tok ] filename
```

By default interpreter does the typecheck, then evaulate a program step-by-step.

Options:
* -h Prints possible flags
* -eval Only evaluating, without typechecking
* -tc Only typechecking
* -tok Prints tokens from lexer

During evaluation and typechecking interpreter prints information about variables in memory and heap.

Unfortunately, it's impossible to turn this off.

Furthermore, there is no info what kind of parsing error there is. I hope code from `examples/` is enough for an oveview.

See `raport.pdf` (in Polish) for more details.
