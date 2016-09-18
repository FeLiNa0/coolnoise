Use Criterion to benchmark:
- flex lexer
- megaparsec lexer (treat as an external program, call `system()`)
- bison parser
- megaparsec parser
- attoparsec parser

Need to implement and describe the following:
- A better counter.
- A concise COOL expression parser with good error messages using megaparsec.
- A concise COOL expression parser using attoparsec.
- Tests for the parser based on expressions from the examples.
- Tests based on randomly generated expressions.

Need to implement and describe code to randomly generate:
- All expressions of a given depth.
- Random expression trees of varying depth and breadth.
- All type checked expressions of a given depth.
- Type checked random expressions.

Maybe include class definitions in the parser and generators.

Maybe use a Haskell implementation of [miniKanren](https://github.com/sergv/hkanren)?
