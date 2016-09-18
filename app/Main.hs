module Main where

import System.IO (hFlush, stdout)

import Data.Monoid
import Options.Applicative

import Cool.Types
import Cool.Parse
import Cool.Lex

data Options = Options { fnames :: [FilePath]
                       , inputs :: [String]
                       } deriving Show

manyFs = Options
  <$> many (strOption (long "file" <> short 'f' <> metavar "FILENAME"
            <> help "Use the contents of these .cl or .cool files."))
  <*> many (strOption (long "input" <> short 'i' <> metavar "COOLSTRING"
            <> help "Use this input string."))

optParser = info (manyFs <**> helper)
  (fullDesc <> header "coolnoise - COOL program lexer, parser and generator")

main :: IO ()
main = do
  options <- execParser optParser
  mapM_ (\fname -> do
           putStrLn (fname ++ ":")
           result <- parseFileWith fname coolLexer
           printLexResult result) (fnames options)
  mapM_ (\input -> do
           putStrLn ("<stdin> '" ++ input ++ "':")
           let result = parseStringWith "<stdin>" input coolLexer
           printLexResult result) (inputs options)
