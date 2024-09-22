module Simala.Json.Parser where

import Base
import Simala.Expr.Type

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap

jsonToExpr :: Value -> Expr
jsonToExpr (Object o)   = Record ((\ (k, v) -> (Key.toText k, jsonToExpr v)) <$> KeyMap.toList o)
jsonToExpr (Array a)    = mkList (jsonToExpr <$> toList a)
jsonToExpr (String txt) = Atom (MkAtom txt)
jsonToExpr (Number n)   = Lit (IntLit (floor n))
jsonToExpr (Bool b)     = Lit (BoolLit b)
jsonToExpr Null         = Undefined

jsonToDecls :: Value -> Maybe [Decl]
jsonToDecls v =
  case jsonToExpr v of
    Record row -> pure ((\ (n, e) -> NonRec Opaque n e) <$> row)
    _          -> Nothing
