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

parseWithMega :: a -> Expr b
parseWithMega = undefined

parseWithAtto :: a -> Expr b
parseWithAtto = undefined

parseFileWith file parser = do f <- readFile file
                               return (parseStringWith file f parser)

parseStringWith :: FilePath -> String -> Parser a -> Either String a
parseStringWith fname str parser =
  case parse parser fname str of
    Left e -> Left (parseErrorPretty e)
    Right r -> Right r
