module Simala.Json.Parser where

import Base
import Simala.Expr.Type

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Simala.Expr.Metadata (emptyMeta)

jsonToExpr :: Value -> Expr
jsonToExpr (Object o)   = Record emptyMeta (Rows emptyMeta $ rowFromTuple <$> KeyMap.toList o)
jsonToExpr (Array a)    = mkList emptyMeta (jsonToExpr <$> toList a)
jsonToExpr (String txt) = Atom emptyMeta (Variable emptyMeta txt)
jsonToExpr (Number n)   = Lit emptyMeta (IntLit (floor n))
jsonToExpr (Bool b)     = Lit emptyMeta (BoolLit b)
jsonToExpr Null         = Undefined emptyMeta

jsonToDecls :: Value -> Maybe [Decl]
jsonToDecls v =
  case jsonToExpr v of
    Record _ (Rows _ rows) -> pure ((\(Row _ var e) -> NonRec emptyMeta Opaque var e) <$> rows)
    _          -> Nothing

rowFromTuple :: (Key, Value) -> Row Expr
rowFromTuple (k, v) = Row emptyMeta (Variable emptyMeta (Key.toText k)) (jsonToExpr v)
