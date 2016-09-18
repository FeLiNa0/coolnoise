module Cool.Types (ID, Type, ArithOp, BoolOp, Expr) where

import Data.List.NonEmpty

type ID = String
type Type = String

data ArithOp =
  Add | Sub | Mul | Div
  deriving (Show, Eq)
data BoolOp
  = Lt | Le | Eq
  deriving (Show, Eq)

data Expr = Integer Integer
          | String String
          | Bool Bool
          | ID ID
          | New ID

          | Assign ID  Expr
          | IsVoid     Expr
          | IntNegate  Expr
          | BoolNegate Expr
          | Paren      Expr

          | Op ArithOp Expr Expr
          | BOp BoolOp Expr Expr
          | While      Expr Expr

          | IfThenElse Expr Expr Expr

          | Call ID [Expr]

          | Sequence               (NonEmpty Expr)
          | Static ID (Maybe Type) Expr [Expr]

          | Case           Expr (NonEmpty (ID, Type, Expr))
          | Let            Expr (NonEmpty (ID, Type, Expr))
