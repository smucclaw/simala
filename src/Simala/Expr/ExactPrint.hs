{-# LANGUAGE LambdaCase #-}

module Simala.Expr.ExactPrint where

import Base
import Data.Text qualified as Text
import Simala.Expr.Lexer
import Simala.Expr.Metadata
import Simala.Expr.Type

exactprint :: [Decl] -> Text
exactprint ds = Text.concat $ fmap displayPosToken $ concatMap declToPosTokens ds

exprToTokens :: Expr -> [PosToken]
exprToTokens =
  concat . \case
    Builtin m builtin es -> case builtin of
      Minus -> [infixBuiltin2 m es]
      Divide -> [infixBuiltin2 m es]
      Modulo -> [infixBuiltin2 m es]
      Sum -> [functionOrBuiltin m es]
      Product -> [functionOrBuiltin m es]
      Maximum -> [builtinFunction m es]
      Minimum -> [builtinFunction m es]
      Not -> [builtinFunction m es]
      Lt -> [infixBuiltin2 m es]
      Le -> [infixBuiltin2 m es]
      Gt -> [infixBuiltin2 m es]
      Ge -> [infixBuiltin2 m es]
      Eq -> [infixBuiltin2 m es]
      HEq -> [infixBuiltin2 m es]
      Ne -> [infixBuiltin2 m es]
      And -> [functionOrBuiltin m es]
      Or -> [functionOrBuiltin m es]
      IfThenElse -> case expectArity3 es of
        (a, b, c) ->
          [ tokensAt m 0 -- "if"
          , exprToTokens a
          , tokensAt m 1 -- "then"
          , exprToTokens b
          , tokensAt m 2 -- "else"
          , exprToTokens c
          ]
      Foldl -> [builtinFunction m es]
      Foldr -> [builtinFunction m es]
      Case -> [builtinFunction m es]
      Merge -> [builtinFunction m es]
      Floor -> [builtinFunction m es]
      Ceiling -> [builtinFunction m es]
      FromInt -> [builtinFunction m es]
      Explode -> [builtinFunction m es]
      Append -> [infixBuiltin2 m es]
      TypeOf -> [builtinFunction m es]
      Cons -> [infixBuiltin2 m es]
      List ->
        -- We use "[1, 2, 3]" for list literals, which looks like "(1, 2, 3)",
        -- so we re-use funAppArgs here.
        [funAppArgs m.cstTokens es]
    Var _m var ->
      -- Meta in 'Var' isn't used, yet
      [ variableToPosTokens var
      ]
    Atom m var ->
      [ tokensAt m 0 -- ' before the variable
      , variableToPosTokens var
      ]
    Lit m _l ->
      [ tokensAt m 0 -- The literal itself
      ]
    App m e es ->
      [ exprToTokens e
      , funAppArgs m.cstTokens es
      ]
    Record m rows ->
      [ tokensAt m 0 -- Symbol "{"
      , rowsToPosTokens rows
      , tokensAt m 1 -- Symbol "}"
      ]
    Project m e var ->
      [ exprToTokens e
      , tokensAt m 0 -- Symbol "."
      , variableToPosTokens var
      ]
    Fun m _t args e ->
      [ tokensAt m 0 -- transparency
      , tokensAt m 1 -- "fun"
      , funDeclArgs (drop 2 m.cstTokens) args
      , exprToTokens e
      ]
    Let m d e
      | (ds, inExpr) <- collectLetDecls e ->
          [ tokensAt m 0 -- let
          , declToPosTokens d
          , concatMap declToPosTokens ds
          , tokensAt m 1 -- in
          , exprToTokens inExpr
          ]
    Undefined m ->
      [ tokensAt m 0 -- undefined
      ]
    Parens m e ->
      [ tokensAt m 0 -- Symbol "("
      , exprToTokens e
      , tokensAt m 1 -- Symbol ")"
      ]

collectLetDecls :: Expr -> ([Decl], Expr)
collectLetDecls = go []
  where
    go ds (Let m d e)
      | isEmptyMeta m = go (ds <> [d]) e
    go ds e = (ds, e)

funDeclArgs :: [ClusterMeta] -> [Variable] -> [PosToken]
funDeclArgs = goOpenParen
 where
  goOpenParen (open : cs) vars = csTokens open <> goFirstVar cs vars
  goOpenParen cs vs =
    error $ "funDeclArgs, goOpenParen: " <> show cs <> " and " <> show vs

  goFirstVar cs (v : vars) = variableToPosTokens v <> go cs vars
  goFirstVar cs vs =
    error $ "funDeclArgs, goFirstVar: " <> show cs <> " and " <> show vs

  go [close, arr] [] = csTokens close <> csTokens arr
  go (c : cs) (v : vs) =
    csTokens c <> variableToPosTokens v <> go cs vs
  go cs vs =
    error $ "funDeclArgs, go: " <> show cs <> " and " <> show vs

funAppArgs :: [ClusterMeta] -> [Expr] -> [PosToken]
funAppArgs = goOpenParen
 where
  goOpenParen (open : cs) es = csTokens open <> goFirstVar cs es
  goOpenParen cs es =
    error $ "funAppArgs, goOpenParen: " <> show cs <> " and " <> show es

  goFirstVar [close] [] = csTokens close
  goFirstVar cs (e : es) = exprToTokens e <> go cs es
  goFirstVar cs es =
    error $ "funAppArgs, goFirstVar: " <> show cs <> " and " <> show es

  go [] [] = []
  go [close] [] = csTokens close
  go (c : cs) (e : es) =
    csTokens c <> exprToTokens e <> go cs es
  go cs es =
    error $ "funAppArgs, go: " <> show cs <> " and " <> show es

rowsToPosTokens :: Rows Expr -> [PosToken]
rowsToPosTokens (Rows m rows) = goStart m.cstTokens rows
 where
  goStart cst (r : rs) =
    rowToPosTokens r <> go cst rs
  goStart _ [] = []

  go [] [] = []
  go (c : cs) (r : rs) =
    csTokens c <> rowToPosTokens r <> go cs rs
  go _ _ = error "rowsToPosTokens: unbalanced ',' and rows"

rowToPosTokens :: Row Expr -> [PosToken]
rowToPosTokens (Row m var e) =
  concat
    [ variableToPosTokens var
    , tokensAt m 0 -- Symbol "="
    , exprToTokens e
    ]

variableToPosTokens :: Variable -> [PosToken]
variableToPosTokens (Variable m _name) = tokensAt m 0

declToPosTokens :: Decl -> [PosToken]
declToPosTokens =
  concat . \case
    NonRec m _t name e ->
      [ tokensAt m 0 -- transparency
      , variableToPosTokens name
      , tokensAt m 1 -- Symbol "="
      , exprToTokens e
      , tokensAt m 2 -- Symbol ";"
      ]
    Rec m _t name e ->
      [ tokensAt m 0 -- Keyword "rec"
      , tokensAt m 1 -- transparency
      , variableToPosTokens name
      , tokensAt m 2 -- Symbol "="
      , exprToTokens e
      , tokensAt m 3 -- Symbol ";"
      ]
    Eval m e ->
      [ tokensAt m 0 -- Directive "#eval"
      , exprToTokens e
      , tokensAt m 1 -- Symbol ";"
      ]

infixBuiltin2 :: Meta -> [Expr] -> [PosToken]
infixBuiltin2 m es = concat $ case expectArity2 es of
  (a, b) ->
    [ exprToTokens a
    , tokensAt m 0
    , exprToTokens b
    ]

functionOrBuiltin :: Meta -> [Expr] -> [PosToken]
functionOrBuiltin m es =
  case length m.cstTokens of
    1 -> infixBuiltin2 m es
    _ -> builtinFunction m es

builtinFunction :: Meta -> [Expr] -> [PosToken]
builtinFunction m es =
  concat
    [ tokensAt m 0 -- builtin name
    , funAppArgs (drop 1 m.cstTokens) es
    ]

expectArity2 :: [a] -> (a, a)
expectArity2 [a, b] = (a, b)
expectArity2 as = error $ "expectArity2: Unexpected arity, Expected: 2 but got: " <> show (length as)

expectArity3 :: [a] -> (a, a, a)
expectArity3 [a, b, c] = (a, b, c)
expectArity3 as = error $ "expectArity3: Unexpected arity, Expected: 3 but got: " <> show (length as)
