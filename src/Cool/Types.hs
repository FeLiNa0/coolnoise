module Cool.Types (ID, TypeID, ArithOp(..), BoolOp(..), Expr(..)) where

import Data.List.NonEmpty (NonEmpty((:|)))

type ID = String
type TypeID = String

data ArithOp
  = Add | Sub | Mul | Div
  deriving (Show, Eq, Ord)

data BoolOp
  = Lt | Le | Eq
  deriving (Show, Eq, Ord)

data Expr a 
  = Integer a Integer
  | String  a String
  | Bool    a Bool
  | ID      a ID
  | TypeID  a TypeID
  | New     a TypeID

  | Assign     a ID (Expr a)
  | IsVoid     a    (Expr a)
  | IntNegate  a    (Expr a)
  | BoolNegate a    (Expr a)
  | Paren      a    (Expr a)

  | Op    a ArithOp (Expr a) (Expr a)
  | BOp   a BoolOp  (Expr a) (Expr a)
  | While a         (Expr a) (Expr a)

  | IfThenElse a (Expr a) (Expr a) (Expr a)

  | Call     a ID                [(Expr a)]
  | Sequence a                   (NonEmpty (Expr a))
  | Static   a ID (Maybe TypeID) (Expr a) [(Expr a)]
  | Case     a                   (Expr a) (Bindings a)
  | Let      a                   (Expr a) (Bindings a)
  deriving (Eq, Show, Ord)

type Bindings a = (NonEmpty (ID, TypeID, Expr a))
