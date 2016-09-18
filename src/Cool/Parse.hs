-- test error handling
--
-- TODO *) should throw an error
-- long strings -> throw an error
--
-- \n in string -> ERROR
-- "unterminated string constant"
-- keep lexing at next line
--
-- invalid character -> print error, keep lexing
--
-- EOF in comment -> ERROR
--
-- TODO: lex function takes string -> [token]
-- benchmark Data.Sequence, list (: and reverse)
-- get flex printing tokens in the same way

-- TODO strings
-- TODO whitespace
-- TODO reject all other characters

-- TODO: inline
-- {-# INLINE identChar #-}
--
-- benchmark
module Cool.Parse (parseStringWith, parseFileWith) where

import Text.Megaparsec
import Text.Megaparsec.String

import Cool.Types (Expr)

-- parens
-- method calls
-- control (if while sequence
-- let
-- case

-- binops and prefix unary:
-- .
-- @
-- ~
-- isvoid
-- / *
-- + -
-- <= < =
-- not
-- <-
-- parse

parseWithMega :: a -> Expr
parseWithMega = undefined

parseWithAtto :: a -> Expr
parseWithAtto = undefined

parseFileWith file parser = do f <- readFile file
                               return (parseStringWith file f parser)

parseStringWith :: FilePath -> String -> Parser a -> Either String a
parseStringWith fname str parser =
  case parse parser fname str of
    Left e -> Left (parseErrorPretty e)
    Right r -> Right r
