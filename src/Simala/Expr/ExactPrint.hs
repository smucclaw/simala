{-# LANGUAGE LambdaCase #-}

module Simala.Expr.ExactPrint (exactprint) where

import Base
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Simala.Expr.Lexer
import Simala.Expr.Metadata
import Simala.Expr.Type

exactprint :: [Decl] -> Text
exactprint ds = Text.concat $ fmap displayPosToken $ concatMap declToPosTokens ds

exprToTokens :: Expr -> [PosToken]
exprToTokens = \case
  Builtin m _ es -> applyTokensWithHoles m $ fmap exprToTokens es
  Var m var -> applyTokensWithHoles m [variableToPosTokens var]
  Atom m var -> applyTokensWithHoles m [variableToPosTokens var]
  Lit m _l -> applyTokensWithHoles m []
  App m e es -> applyTokensWithHoles m $ [exprToTokens e] <> fmap exprToTokens es
  Record m rows -> applyTokensWithHoles m [rowsToPosTokens rows]
  Project m e var -> applyTokensWithHoles m [exprToTokens e, variableToPosTokens var]
  Fun m _t args e -> applyTokensWithHoles m $ concat [fmap variableToPosTokens args, [exprToTokens e]]
  Let m d e
    | (ds, inExpr) <- collectLetDecls e ->
        applyTokensWithHoles m ([concatMap declToPosTokens (d : ds)] <> [exprToTokens inExpr])
  Undefined m -> applyTokensWithHoles m []
  Parens m e -> applyTokensWithHoles m [exprToTokens e]

collectLetDecls :: Expr -> ([Decl], Expr)
collectLetDecls = go []
 where
  go ds (Let m d e)
    | isEmptyMeta m = go (ds <> [d]) e
  go ds e = (ds, e)

rowsToPosTokens :: Rows Expr -> [PosToken]
rowsToPosTokens (Rows m rows) = applyTokensWithHoles m $ fmap rowToPosTokens rows

rowToPosTokens :: Row Expr -> [PosToken]
rowToPosTokens (Row m var e) = applyTokensWithHoles m [variableToPosTokens var, exprToTokens e]

variableToPosTokens :: Variable -> [PosToken]
variableToPosTokens (Variable m _name) = applyTokensWithHoles m []

declToPosTokens :: Decl -> [PosToken]
declToPosTokens = \case
  NonRec m _t name e -> applyTokensWithHoles m [variableToPosTokens name, exprToTokens e]
  Rec m _t name e -> applyTokensWithHoles m [variableToPosTokens name, exprToTokens e]
  Eval m e -> applyTokensWithHoles m [exprToTokens e]

type HoleFit = [PosToken]

applyTokensWithHoles :: (HasCallStack) => Meta -> [HoleFit] -> [PosToken]
applyTokensWithHoles (Meta []) _ = []
applyTokensWithHoles (Meta (MetaHole : cs)) holeFits = case holeFits of
  [] -> error $ "applyTokensWithHoles: HoleFit requested, but not enough Fits given."
  (x : xs) -> x <> applyTokensWithHoles (Meta cs) xs
applyTokensWithHoles (Meta (MetaCsn m : cs)) xs = csTokens m <> applyTokensWithHoles (Meta cs) xs
