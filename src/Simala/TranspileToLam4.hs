module Simala.TranspileToLam4 where

import Base
import qualified Base.Text as Text

import Simala.Expr.Render
import Simala.Expr.Type

-- POSSIBLE TODOs:
-- - scan for atom declarations, or assume global mapping?
-- - group nested let bindings?
-- - layout

--
--
-- An attempt at inferring operator priorities from the Langium grammar:
--
-- Expr: OrExpr | LetExpr | RecordExpr
-- OrExpr: 
--
--
--    (_) | ?anon_function | EMPTY_LIST | LIST_OF ... | ?foldr ...
--    _(_)   (function application)
-- 9  followed_by_items_in    (cons)
-- 8  's   (projection)
-- 7  * | /
-- 6  + | -
-- 4  < | <= | > | >= | (EQUALS | ==) | (DIFFERS_FROM | !=) | IF_THEN_ELSE
-- 3  AND
-- 2  OR

class AsLam4 a where

  lam4AtPrio :: Int -> a -> Text
  lam4AtPrio _ = lam4

  lam4 :: a -> Text
  lam4 = lam4AtPrio 0

  {-# MINIMAL lam4AtPrio | lam4 #-}

instance AsLam4 Expr where
  lam4AtPrio :: Int -> Expr -> Text
  lam4AtPrio p (Builtin b es)  = lam4Builtin p b es
  lam4AtPrio _ (Var x)         = lam4 x
  lam4AtPrio _ (Atom x)        = lam4 (atomMapping x)
  lam4AtPrio _ (Lit l)         = lam4 l
  lam4AtPrio p (Cons e1 e2)    = lam4Binop 9 " followed_by_items_in " p e1 e2
  lam4AtPrio _ (List [])       = "EMPTY_LIST"
  lam4AtPrio _ (List xs)       = "LIST_OF " <> lam4Commas xs
  lam4AtPrio _ (Record r)      = "{| " <> lam4Row r <> " |}"
  lam4AtPrio p (Project e n)   = parensIf (p > 8) (lam4AtPrio 9 e <> "'s " <> lam4 n)
  lam4AtPrio p (Fun _t args e) = parensIf (p > 0) ("\\ " <> Text.intercalate " " (map lam4 args) <> " => " <> lam4 e)
  lam4AtPrio p (Let d e)       = parensIf (p > 0) ("LET { " <> lam4Inner d <> " } IN { " <> lam4AtPrio 1 e <> " }")
  lam4AtPrio p (App e es)      = parensIf (p > 10) (lam4AtPrio 11 e <> "(" <> lam4Commas es <> ")")
  lam4AtPrio _ Undefined       = lam4 (atomMapping "undefined")

lam4Builtin :: Int -> Builtin -> [Expr] -> Text
lam4Builtin p Minus      [e1, e2]     = lam4Binop 6 " - "  p e1 e2
lam4Builtin p Sum        [e1, e2]     = lam4Binop 6 " + "  p e1 e2
lam4Builtin p Product    [e1, e2]     = lam4Binop 7 " * "  p e1 e2
lam4Builtin p Divide     [e1, e2]     = lam4Binop 7 " / "  p e1 e2
lam4Builtin p Ge         [e1, e2]     = lam4Binop 4 " >= " p e1 e2
lam4Builtin p Le         [e1, e2]     = lam4Binop 4 " <= " p e1 e2
lam4Builtin p Gt         [e1, e2]     = lam4Binop 4 " > "  p e1 e2
lam4Builtin p Lt         [e1, e2]     = lam4Binop 4 " < "  p e1 e2
lam4Builtin p Eq         [e1, e2]     = lam4Binop 4 " EQUALS " p e1 e2
lam4Builtin p HEq        [e1, e2]     = lam4Binop 4 " EQUALS " p e1 e2
lam4Builtin p Ne         [e1, e2]     = lam4Binop 4 " DIFFERS_FROM " p e1 e2
lam4Builtin p And        [e1, e2]     = lam4Binop 3 " AND " p e1 e2
lam4Builtin p Or         [e1, e2]     = lam4Binop 2 " OR " p e1 e2
lam4Builtin p IfThenElse [e1, e2, e3] =
  parensIf (p > 0) ("IF " <> lam4 e1 <> " THEN " <> lam4 e2 <> " ELSE " <> lam4 e3)
lam4Builtin p Not        [e]          = parensIf (p > 10) ("NOT " <> lam4 e)
lam4Builtin p Foldr      [e1, e2, e3] = parensIf (p > 0) ("FOLD_RIGHT " <> "using " <> lam4 e1 <> " starting_with " <> lam4 e2 <> " over " <> lam4 e3)
lam4Builtin p FromInt    [e]          = parensIf (p > 10) (lam4SimalaFunApp "FromInt" [e])
lam4Builtin p Floor      [e]          = parensIf (p > 10) (lam4SimalaFunApp "Floor" [e])
lam4Builtin _ b          _es          = error $ "unspported builtin: " <> show b

lam4SimalaFunApp :: Text -> [Expr] -> Text
lam4SimalaFunApp x es = x <> "(" <> lam4Commas es <> ")" 

lam4Inner :: Decl -> Text
lam4Inner (Rec _ _ _)    = error "uncertain if recursive inner decls are supported"
lam4Inner (NonRec _ x e) = lam4 x <> " = " <> lam4 e
lam4Inner (Eval _)       = error "inner eval directive are not supported"

instance AsLam4 Decl where
  lam4 :: Decl -> Text
  lam4 (Rec _ x (Fun _ args e)) = "FUNCTION Any " <> lam4 x <> "(" <> lam4Commas args <> ") = " <> lam4 e <> " END"
  lam4 (Rec _ x e)              = "DEFINE " <> lam4 x <> " = " <> lam4 e -- TODO / unclear
  lam4 (NonRec _ x e)           = "DEFINE " <> lam4 x <> " = " <> lam4 e
  lam4 (Eval e)                 = "@REPORT " <> lam4 e

instance AsLam4 Name where
  lam4 :: Name -> Text
  lam4 = renderName

instance AsLam4 Lit where
  lam4 :: Lit -> Text
  lam4 (IntLit i)      = Text.pack (show i)
  lam4 (BoolLit True)  = "True"
  lam4 (BoolLit False) = "False"
  lam4 (StringLit s)   = lam4 (atomMapping s)
  lam4 (FracLit f)     = Text.pack (show f)

atomMapping :: Text -> Text
atomMapping "unknown" = "Unknown"
atomMapping "uncertain" = "Uncertain"
atomMapping "undefined" = "Undefined"
atomMapping x = x -- TODO: unsafe

lam4Binopl :: (AsLam4 a1, AsLam4 a2) => Int -> Text -> Int -> a1 -> a2 -> Text
lam4Binopl t txt p e1 e2 = parensIf (p > t) (lam4AtPrio t e1 <> txt <> lam4AtPrio (t + 1) e2)

lam4Binopr :: (AsLam4 a1, AsLam4 a2) => Int -> Text -> Int -> a1 -> a2 -> Text
lam4Binopr t txt p e1 e2 = parensIf (p > t) (lam4AtPrio (t + 1) e1 <> txt <> lam4AtPrio t e2)

lam4Binop :: (AsLam4 a1, AsLam4 a2) => Int -> Text -> Int -> a1 -> a2 -> Text
lam4Binop t txt p e1 e2 = parensIf (p > t) (lam4AtPrio (t + 1) e1 <> txt <> lam4AtPrio (t + 1) e2)

lam4Commas :: AsLam4 a => [a] -> Text
lam4Commas xs = Text.intercalate ", " (map lam4 xs)

lam4Row :: AsLam4 a => Row a -> Text
lam4Row xs = Text.intercalate ", " (map item xs)
  where
    item (x, a) = lam4 x <> " = " <> lam4 a
