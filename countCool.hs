-- TODO: count any mnumber of subexpre in each slot
-- example: App (expr of depth N) (expr of depth M)
import Data.List

groupsize n [] = [] 
groupsize n l = (take n l) : groupsize n (drop n l)

{- Converts an integer into a string. Adds commas between digit triplets.
 - Example:
 - showInt 1000000 -> 1,000,000
-}
showInt :: (Show a, Integral a) => a -> String
showInt = concat . map reverse . reverse . Data.List.intersperse "," . groupsize 3 . reverse . show

{- How many sub-expressions does an expression need?
 -
 - Counts the number of expressions if the maximum number of variable 
 - subexpressions is `maxvar`. 
 -
 - Let expressions take at least two sub-expressions.
 -
 - ```
 - let a1 <- expr [, an <- expr]* in expr
 - ```
 -
 - If `maxvar` is set to 3, this function includes let-expressions that
 - bind 1, 2, 3, or 4 variables. 
 -} 
numExpr maxvar = 5  -- 5 take 1 subexpr
  + 8 * 2  -- 8 take 2 subexprs
  + 1 * 3  -- 1 takes 3 subexprs
  + sum [n * 1        -- 1 takes at least 0 subexprs
       + (n + 1) * 2  -- 2 take at least 1 subexprs 
       + (n + 2) * 2  -- 2 take at least 2 subexprs 
       | n<-[0..maxvar]]
 
totalNodes maxvar maxdepth = 
  sum [(numExpr maxvar) ^ d | d <- [0..maxdepth]] * 6  -- 6 expressions have depth 0


