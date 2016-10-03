-- two strategies:
--
-- genenerate all expressions up to a certain depth with a maximum number of variable arguments
--
-- generate a random expression tree with a constant number of nodes
-- expression types, tree depth, and tree breadth can be adjusted by passing in different expression pickers
module Cool.Generate (generateAll, RandomData(..)) where

import Cool.Types
import Data.List.NonEmpty (NonEmpty(..))

data RandomData = Empty | HasData deriving (Show, Eq, Ord)

l `cross` n = sequence $ replicate n l

generateAll :: Int -> Int -> [Expr RandomData]
generateAll 0 _ = map ($Empty) arg0
generateAll depth maxbreadth =
 let es = concat [generateAll d maxbreadth | d <- [0..depth-1]]
 in concat $ take maxbreadth $
 [ [f Empty e | f <- arg1, e <- es]
 , [f Empty e1 e2 | f <- arg2, (e1:e2:[]) <- es `cross` 2]
 , [f Empty e1 e2 e3 | f <- arg3, (e1:e2:e3:[]) <- es `cross` 3]
 ] ++
 [ [f Empty e1 e2 ess | f <- arg2plus, (e1:e2:ess) <- es `cross` n] | n <- [4..] ]

defaultArith  = Add
defaultBoolOp = Lt
defaultID     = "a"
defaultTypeID = "A"
defaultBinding e = (defaultID, defaultTypeID, e)

arg0 =
  [ \tag -> Integer tag 0
  , \tag -> String  tag ""
  , \tag -> Bool    tag False
  , \tag -> ID      tag defaultID
  , \tag -> TypeID  tag defaultTypeID
  , \tag -> New     tag defaultTypeID ]

arg1 :: [a -> Expr a -> Expr a]
arg1 =
  [ \tag -> Assign     tag defaultID
  , \tag -> IsVoid     tag
  , \tag -> IntNegate  tag
  , \tag -> BoolNegate tag
  , \tag -> Paren      tag
  , \tag e -> Call     tag defaultID [e]
  , \tag e -> Sequence tag (e :| [])
  , \tag e -> Static   tag defaultID (Just defaultTypeID) e []
  ]

arg2plus :: [a -> Expr a -> Expr a -> [Expr a] -> Expr a]
arg2plus =
  [ \tag e1 e2 es -> Call     tag defaultID $ [e1, e2] ++ es
  , \tag e1 e2 es -> Sequence tag $ e1 :| (e2:es)
  , \tag e1 e2 es -> Static   tag defaultID (Just defaultTypeID) e1 (e2:es)
  , \tag e1 e2 es -> Case     tag e1 $ (defaultBinding e2) :| map defaultBinding es
  , \tag e1 e2 es -> Let      tag e1 $ (defaultBinding e2) :| map defaultBinding es
  ]

arg2 :: [a -> Expr a -> Expr a -> Expr a]
arg2 =
  [ \tag -> Op    tag defaultArith
  , \tag -> BOp   tag defaultBoolOp
  , While
  ] ++ (map (\f -> \tag e1 e2 -> f tag e1 e2 []) arg2plus)

arg3 :: [a -> Expr a -> Expr a -> Expr a -> Expr a]
arg3 =
  [ IfThenElse
  ] ++ (map (\f -> \tag e1 e2 e3 -> f tag e1 e2 [e3]) arg2plus)
