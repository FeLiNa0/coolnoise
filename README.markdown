# COOLNOISE
This Haskell code implements a parser and a generator for programs written in the 
[COOL language](http://theory.stanford.edu/~aiken/software/cool/cool.html).

| File           | Purpose                                                 |
|----------------|---------------------------------------------------------|
| `countCool.hs` | Counts the number of COOL expressions of a given depth. |

## Installing
This project uses [stack](http://docs.haskellstack.org/en/stable/README/).

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
