-- measure runtimes of any number of programs that take a COOL filename as input and lex or parse it
module Main where

import Criterion.Main (defaultMain, nfIO, nf, bench)
import Cool.Lex (coolLexer)
import Cool.Parse (parseFileWith)
import System.Process (readProcess)

argsFor fnames = words $ "exec coolnoise-exe -- "

testFiles =
     words "arith.cl atoi.cl atoi_test.cl book_list.cl cells.cl io.cl lam.cl"
  ++ words "complex.cl cool.cl dlist.cl graph.cl hairyscary.cl hello_world.cl"
  ++ words "life.cl list.cl new_complex.cl palindrome.cl primes.cl sort_list.cl"

lexEachWithExec = map (\fname -> bench ("coolnoise-exe (" ++ fname ++ ")")
                  $ nfIO (readProcess "stack" (argsFor [fname]) ""))

lexEachWithLib =  map (\fname -> bench ("Cool.Lex (" ++ fname ++ ")")
                  $ nfIO $ parseFileWith fname coolLexer)

lexAllWithLib fnames = [bench "coolnoise-exe" $ nfIO $ readProcess "stack" (argsFor fnames) ""]

lexAllWithExec fnames = [bench "Cool.Lex" $ nfIO $ mapM_ (\f -> parseFileWith f coolLexer) fnames]

main = defaultMain $ lexAllWithLib testFiles ++ lexAllWithExec testFiles
                  ++ lexEachWithLib testFiles
                  -- interleave (lexEachWithExec testFiles) (lexEachWithLib testFiles)
  where files = words "lam.cl hello_world.cl"
        interleave l1 l2 = concat $ zipWith (\a b -> [a,b]) l1 l2
