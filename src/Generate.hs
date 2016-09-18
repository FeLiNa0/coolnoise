-- two strategies:
--
-- genenerate all expressions up to a certain depth with a maximum number of variable arguments
--
-- generate a random expression tree with a constant number of nodes
-- expression types, tree depth, and tree breadth can be adjusted by passing in different expression pickers
module Generate (generateAll) where

import Types (Expr)

generateAll :: Int -> Expr
generateAll = undefined
