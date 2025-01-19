<a name="readme-top"></a>

# OCamlPrintf

`OCaml` interpreter with support for printf.

#### MiniML

- Standard data types: `bool`, `int`, `tuples`, `list` and `option`;
- Recursive functions;
- Comparisons of numbers and other arithmetic;
- Standard functions.

#### Printf

- Support for char, string types and operations with them;
- Support the formatted printing function.

## Progress

- [x] [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
  - [x] Factorial
  - [x] MiniML
- [x] Parser
  - [x] Factorial
  - [x] MiniML
- [x] [REPL](https://en.wikipedia.org/wiki/Read–eval–print_loop)
- [x] Pretty Printer
- [x] [Quick Checker](https://en.wikipedia.org/wiki/QuickCheck)
  - [x] Manual
  - [x] Auto
- [x] Shrinker
- [x] Type Checker
- [x] Interpreter
- [ ] Support for char, string types and operations with them
  - [x] Types
  - [x] Comparison
  - [x] Concatenation
- [ ] Support the formatted printing function

## Build

```shell
cd OCamlPrintf/
dune build # Build the project.
```

## Run

```shell
dune runtest # Run all tests.
dune exec -- repl/REPL.exe -dparsetree -fromfile tests/factorial.txt # Run parser tests and see AST.
dune exec -- tests/run_qchecker.exe -v # Run qchecker tests with verbose mode.
dune exec -- repl/REPL.exe -inference # Run inferencer in REPL.
dune exec repl/REPL.exe # Run interpreter.
```

## Authors

- [@Friend-zva](https://github.com/Friend-zva) (Vladimir Zaikin)
- [@RodionovMaxim05](https://github.com/RodionovMaxim05) (Maxim Rodionov)

## License

Distributed under the GNU GENERAL PUBLIC LICENSE. See [lisence](COPYING) for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>
