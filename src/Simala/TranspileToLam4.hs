{-# LANGUAGE DataKinds #-}
module Simala.TranspileToLam4 where

import Base
import qualified Base.Text as Text

import Simala.Expr.Render
import Simala.Expr.Type

import Numeric (showFFloat)
import Optics ((%))
import Optics.Fold (toListOf)
import Optics.Generic (gconstructor, gplate)
import Prettyprinter
import Prettyprinter.Render.Text

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

  lam4AtPrio :: Int -> a -> Doc ann
  lam4AtPrio _ = lam4

  lam4 :: a -> Doc ann
  lam4 = lam4AtPrio 0

  {-# MINIMAL lam4AtPrio | lam4 #-}

lam4AsText :: AsLam4 a => a -> Text
lam4AsText = renderStrict . layoutPretty defaultLayoutOptions . lam4

instance AsLam4 Expr where
  lam4AtPrio :: Int -> Expr -> Doc ann
  lam4AtPrio p (Builtin b es)    = lam4Builtin p b es
  lam4AtPrio _ (Var x)           = lam4 x
  lam4AtPrio _ (Atom (MkAtom x)) = lam4 (atomMapping x)
  lam4AtPrio _ (Lit l)           = lam4 l
  lam4AtPrio _ (Record r)        = "{|" <+> lam4Row r <+> "|}"
  lam4AtPrio p (Project e n)     = parensIf (p > 8) (lam4AtPrio 9 e <> "'s" <+> lam4 n)
  lam4AtPrio p (Fun _t args e)   =
    parensIf (p > 0) (align (nest 4 (sep ["\\" <+> hsep (map lam4 args) <+> "=>", lam4 e])))
  lam4AtPrio p (Let d e)         =
    let
      (decls, body) = scanLet (Let d e)
    in
      parensIf (p > 0)
        (align (sep
          ["LET" <+> "{" <+> align (vsep (punctuate "," (lam4Inner <$> decls))) <+> "}", "IN" <+> "{" <+> lam4AtPrio 1 body <+> "}"]
      ))
  lam4AtPrio p (App e es)        = parensIf (p > 10) (lam4AtPrio 11 e <> "(" <> lam4Commas es <> ")")
  lam4AtPrio _ Undefined         = lam4 (atomMapping "undefined")

lam4Builtin :: Int -> Builtin -> [Expr] -> Doc ann
lam4Builtin _p List      []           = "EMPTY_LIST "
lam4Builtin _p List      es           = "LIST_OF " <> lam4Commas es
lam4Builtin p Minus      [e1, e2]     = lam4Binop 6 "-"  p e1 e2
lam4Builtin p Sum        [e1, e2]     = lam4Binopl 6 "+"  p e1 e2
lam4Builtin p Product    [e1, e2]     = lam4Binopl 7 "*"  p e1 e2
lam4Builtin p Divide     [e1, e2]     = lam4Binop 7 "/"  p e1 e2
lam4Builtin p Ge         [e1, e2]     = lam4Binop 4 ">=" p e1 e2
lam4Builtin p Le         [e1, e2]     = lam4Binop 4 "<=" p e1 e2
lam4Builtin p Gt         [e1, e2]     = lam4Binop 4 ">"  p e1 e2
lam4Builtin p Lt         [e1, e2]     = lam4Binop 4 "<"  p e1 e2
lam4Builtin p Eq         [e1, e2]     = lam4Binop 4 "EQUALS" p e1 e2
lam4Builtin p HEq        [e1, e2]     = lam4Binop 4 "EQUALS" p e1 e2
lam4Builtin p Ne         [e1, e2]     = lam4Binop 4 "DIFFERS_FROM" p e1 e2
lam4Builtin p And        [e1, e2]     =
  -- NOTE: Or currently right-associative in Simala and left-associative in Lam4, but
  -- it should not matter
  let
    clauses = scanAnd e2
  in
    parensIf (p > 3) (align (sep
      (  [gindent 4 (lam4AtPrio 4 e1)]
      ++ concat ((\ e -> ["AND" <+> lam4AtPrio 4 e]) <$> clauses)
      )
    ))
lam4Builtin p Or         [e1, e2]     =
  -- NOTE: Or currently right-associative in Simala and left-associative in Lam4, but
  -- it should not matter
  let
    clauses = scanOr e2
  in
    parensIf (p > 2) (align (sep
      (  [gindent 3 (lam4AtPrio 3 e1)]
      ++ concat ((\ e -> ["OR" <+> lam4AtPrio 3 e]) <$> clauses)
      )
    ))
lam4Builtin p IfThenElse [e1, e2, e3] =
  let
    (conditions, fallback) = scanIfThenElse e3
  in
   parensIf (p > 0) (align (sep
     (  ["IF" <+> lam4AtPrio 2 e1, "THEN" <+> lam4AtPrio 2 e2]
     ++ concat ((\ (c, e) -> ["ELSE IF" <+> lam4AtPrio 2 c, "THEN" <+> lam4AtPrio 2 e]) <$> conditions)
     ++ ["ELSE" <+> lam4AtPrio 2 fallback]
     )
   ))
lam4Builtin p Not        [e]          = parensIf (p > 10) ("NOT" <+> lam4 e)
lam4Builtin p Foldr      [e1, e2, e3] =
  parensIf (p > 0) (align (sep ["FOLD_RIGHT", "using" <+> lam4 e1, "starting_with" <+> lam4 e2, "over" <+> lam4 e3]))
lam4Builtin p FromInt    [e]          = parensIf (p > 10) (lam4SimalaFunApp "integer_to_fraction" [e])
lam4Builtin p Floor      [e]          = parensIf (p > 10) (lam4SimalaFunApp "floor" [e])
lam4Builtin _ b          _es          = error $ "unspported builtin: " <> show b

scanAnd :: Expr -> [Expr]
scanAnd (Builtin And [e1, e2]) =
  e1 : scanOr e2
scanAnd e = [e]

scanOr :: Expr -> [Expr]
scanOr (Builtin Or [e1, e2]) =
  e1 : scanOr e2
scanOr e = [e]

scanIfThenElse :: Expr -> ([(Expr, Expr)], Expr)
scanIfThenElse (Builtin IfThenElse [e1, e2, e3]) =
  let
    (conditions, fallback) = scanIfThenElse e3
  in
    ((e1, e2) : conditions, fallback)
scanIfThenElse e = ([], e)

lam4SimalaFunApp :: Doc ann -> [Expr] -> Doc ann
lam4SimalaFunApp x es = x <> "(" <> lam4Commas es <> ")"

lam4Inner :: Decl -> Doc ann
lam4Inner (Rec _ _ _)    = error "uncertain if recursive inner decls are supported"
lam4Inner (NonRec _ x e) = align (nest 4 (sep [lam4 x <+> "=", lam4 e]))
lam4Inner (Eval _)       = error "inner eval directive are not supported"

instance AsLam4 Decl where
  lam4 :: Decl -> Doc ann
  lam4 (Rec _ x (Fun _ args e)) = vsep ["FUNCTION Any", lam4 x <> "(" <> lam4Commas args <> ")" <+> "=" <+> lam4 e, "END"]
  lam4 (Rec _ x e)              = align (nest 4 (sep ["DEFINE" <+> lam4 x <+> "=", lam4 e])) -- TODO / unclear
  lam4 (NonRec _ x e)           = align (nest 4 (sep ["DEFINE" <+> lam4 x <+> "=", lam4 e]))
  lam4 (Eval e)                 = "@REPORT" <+> lam4 e

instance AsLam4 Name where
  lam4 :: Name -> Doc ann
  lam4 = pretty . lam4Name

instance AsLam4 Lit where
  lam4 :: Lit -> Doc ann
  lam4 (IntLit i)      = pretty (show i)
  lam4 (BoolLit True)  = "True"
  lam4 (BoolLit False) = "False"
  lam4 (StringLit s)   = pretty (atomMapping s) -- pretty (show s), depending on whether Lam4 supports strings properly
  lam4 (FracLit f)     = pretty (showFFloat Nothing f "") -- (ceiling f :: Int))

atomMapping :: Text -> Text
atomMapping "unknown" = "Unknown"
atomMapping "uncertain" = "Uncertain"
atomMapping "undefined" = "Undefined"
atomMapping x = x -- TODO: unsafe

lam4Name :: Name -> Text
lam4Name n
  | needsQuoting n = "`" <> n <> "`" -- lam4Mangle n -- "`" <> n <> "`"
  | otherwise      = n

lam4Mangle :: Name -> Text
lam4Mangle =
    Text.replace "'" "_"
  . Text.replace "-" "_"
  . Text.replace "(" "_"
  . Text.replace ")" "_"
  . Text.replace " " "_"

lam4Binopl :: (AsLam4 a1, AsLam4 a2) => Int -> Text -> Int -> a1 -> a2 -> Doc ann
lam4Binopl t txt p e1 e2 = parensIf (p > t) (align (sep [gindent (Text.length txt + 1) (lam4AtPrio t e1), pretty txt <+> lam4AtPrio (t + 1) e2]))

lam4Binopr :: (AsLam4 a1, AsLam4 a2) => Int -> Text -> Int -> a1 -> a2 -> Doc ann
lam4Binopr t txt p e1 e2 = parensIf (p > t) (align (sep [gindent (Text.length txt + 1) (lam4AtPrio (t + 1) e1), pretty txt <+> lam4AtPrio t e2]))

lam4Binop :: (AsLam4 a1, AsLam4 a2) => Int -> Text -> Int -> a1 -> a2 -> Doc ann
lam4Binop t txt p e1 e2 = parensIf (p > t) (align (sep [gindent (Text.length txt + 1) (lam4AtPrio (t + 1) e1), pretty txt <+> lam4AtPrio (t + 1) e2]))

lam4Commas :: AsLam4 a => [a] -> Doc ann
lam4Commas xs = align (sep (punctuate "," (map lam4 xs)))

lam4Row :: AsLam4 a => Row a -> Doc ann
lam4Row xs = align (sep (punctuate "," (map item xs)))
  where
    item (x, a) = lam4 x <+> "=" <+> lam4 a

doToLam4 :: [Decl] -> Text
doToLam4 decls =
  let
    stringLits :: [Text]
    stringLits = nub (toListOf (gplate @Lit % gconstructor @"StringLit") decls)

    atoms :: [Atom]
    atoms = nub (toListOf (gplate @Atom) decls ++ (MkAtom <$> stringLits))
  in
    prelude <> Text.unlines (concept <$> atoms) <> Text.unlines (lam4AsText <$> decls)

concept :: Atom -> Text
concept (MkAtom n) = "ONE CONCEPT " <> lam4Name (atomMapping n) <> " END"

prelude :: Text
prelude = Text.unlines
  [ "STRUCTURE Any END"
  ]
