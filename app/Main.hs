module Main where

import Data.Char (toLower)
import Cool.Types
import Cool.Parse

main :: IO ()
main = do
  putStr "Enter filename > "
  fname <- getLine
  parseFileWith fname coolLexer >>= printLexResult 
