{-# LANGUAGE OverloadedStrings #-}
module Cool.PrettyPrint (Cool.PrettyPrint.render) where
import Data.List.NonEmpty (toList)
import Text.PrettyPrint as P
import Cool.Types

render :: Show a => Int -> Int -> Expr a -> String
render lineLength indentation = renderStyle (Style PageMode lineLength ribbonsPerLine) . r
  where
    ribbonsPerLine = 2.0  -- maximum of lineLength/2 characters per line

    args es  = hcat $ punctuate (text ", ") $ map r es
    semis f = vcat . map (nest indentation . (<> semi) . f)

    rendercase (i, t, e) = text i <> colon <> text t <+> "=>" <+> r e
    renderlet  (i, t, e) = text i <> colon <> text t <+> "<-" <+> r e

    arithop Add = "+"
    arithop Sub = "-"
    arithop Mul = "*"
    arithop Div = "/"
    binop Lt = "<"
    binop Le = "<="
    binop Eq = "="

    r (Integer a i) = integer i
    r (String  a i) = doubleQuotes $ text i
    r (Bool    a i) = if i then "true" else "false"
    r (ID      a i) = text i
    r (TypeID  a i) = text i
    r (New     a i) = "new" <+> text i

    r (Assign     a i e) = text i <+> "<-" <+> r e
    r (IsVoid     a   e) = "isvoid" <+> r e
    r (IntNegate  a   e) = "~" <> r e
    r (BoolNegate a   e) = "not" <+> r e
    r (Paren      a   e) = parens (r e)

    r (Op    a o e1 e2) = (r e1) <+> (arithop o) <+> (r e2)
    r (BOp   a o e1 e2) = (r e1) <+> (binop o) <+> (r e2)
    r (While a   e1 e2) = "while" <+> r e1 <+> "loop" <+> r e2 <+> "pool"

    r (IfThenElse a e1 e2 e3) =
      "if" <+> r e1 <+> "then" <+> r e2 <+> "else" <+> r e3 <+> "fi"

    r (Call     a i      es) = text i <> parens (args es)
    r (Sequence a        es) = braces (semis r $ toList es)
    r (Static   a i mT e es) =
      r e <> maybe empty (\t -> "@" <> text t) mT <> "." <> text i <> parens (args es)
    r (Case     a      e bs) =
      "case" <+> r e <+> "of" <+> semis rendercase (toList bs) $$ "esac"
    r (Let      a      e bs) =
      "let" $$ (vcat . punctuate ", " . map renderlet) (toList bs) $$ "in" <+> r e
