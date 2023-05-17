# COOLNOISE
This Haskell code implements a parser and a generator for programs written in the 
[COOL language](http://theory.stanford.edu/~aiken/software/cool/cool.html).

This was used for fuzz testing the first two weeks' work of my project as a senior year student
at my alma mater, the New Mexico Institute of Mining and Technology
(New Mexico Tech).

| File           | Purpose                                                 |
|----------------|---------------------------------------------------------|
| `countCool.hs` | Counts the number of COOL expressions of a given depth. |

## Installation and Compilation
This project uses [stack](http://docs.haskellstack.org/en/stable/README/) and
the Haskell LTS 7.0 distribution. [Read these download and installation
instructions for stack.](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

If you have not installed GHC 8.0.1 yet, run 

```
stack setup
```

This command will download all dependencies and compile the project:

```
stack build
```

## Examples
### Lexer

After compiling the program, run the following command to see the help file:

```
stack exec coolnoise-exe -- --help
```

Use the `--file` option to lex a file:

```
stack exec coolnoise-exe -- --file cool_examples/hello_world.cl
```

You can also run the lexer on a string with the `--input` option:

```
stack exec coolnoise-exe -- --input '(* sample *) main() : SELF_TYPE { out_string("HI") };' 
```

### Parser

### Generator

## Tests

None yet.

## Benchmarks

To benchmark the lexer, run

```
stack build 
stack exec bench-cli -- --output benchmark.html 
```

Open `benchmark.html` in a browser to see the results.

## Number of Expressions

There are at least 224,974,626 possible COOL untyped expression
that have depth of 4, and contain at most 7 subexpressions
(for example, a let-expression that binds 6 variables contains 7 subexpressions).
This table summarizes results with the same number of subexpressions:

| Depth | Number of COOL Expressions (not type checked) |
|-------|----------------------------|
| 0     | 6
| 1     | >= 747
| 2     | >= 36,978
| 3     | >= 2,884,290
| 4     | >= 224,974,626
