# COOLNOISE
This Haskell code implements a parser and a generator for programs written in the 
[COOL language](http://theory.stanford.edu/~aiken/software/cool/cool.html).

| File           | Purpose                                                 |
|----------------|---------------------------------------------------------|
| `countCool.hs` | Counts the number of COOL expressions of a given depth. |

## Results

There are at least 224,974,626 possible COOL expression
that are not type checked, have depth of 4, and take at most 7 subexpressions
(for example, a let-expression that binds 6 variables takes 7 subexpressions).
This table summarizes results with the same number of subexpressions:

| Depth | Number of COOL Expressions |
|-------|----------------------------|
| 0     | 6
| 1     | 747
| 2     | 36,978
| 3     | 2,88,290
| 4     | 224,974,626
