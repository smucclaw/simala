module Simala.Expr.Type where

import Base

type Name = Text

data Expr =
    Builtin    Builtin [Expr]              -- built-ins; the currently decide their own eval strategy, so can be used for control flow
  | Var        Name
  | Lit        Lit
  | Cons       Expr Expr                   -- should be a built-in?
  | List       [Expr]                      -- construct a list
  | App        Expr [Expr]                 -- function application
  | Fun        Transparency [Name] Expr    -- anonymous function
  | Let        Transparency Name Expr Expr -- local declaration
  | Undefined                              -- unclear
  deriving stock Show

data Transparency =
    Transparent
  | Opaque
  deriving stock Show

data Lit =
    IntLit     Int
  | BoolLit    Bool           -- could also be done using two nullary built-ins
  deriving stock Show

data Builtin =
    Minus      -- arity 2
  | Divide     -- arity 2
  | Modulo     -- arity 2
  | Sum        -- flexible arity
  | Product    -- flexible arity
  | Maximum    -- arity > 0
  | Minimum    -- arity > 0
  | Not        -- arity 1
  | Lt         -- arity 2
  | Le         -- arity 2
  | Gt         -- arity 2
  | Ge         -- arity 2
  | Eq         -- arity 2
  | Ne         -- arity 2
  | And        -- flexible arity
  | Or         -- flexible arity
  | IfThenElse -- arity 3
  | Fold       -- arity 3
  deriving stock Show

data Val =
    VInt  Int
  | VBool Bool
  | VList [Val]
  | VClosure Closure
  deriving stock Show

data ValTy =
    TInt
  | TBool
  | TList
  | TFun
  deriving stock Show

data Closure =
  MkClosure [Name] Expr Env
  deriving stock Show

type Env = Map Name Val

valTy :: Val -> ValTy
valTy (VInt _)     = TInt
valTy (VBool _)    = TBool
valTy (VList _)    = TList
valTy (VClosure _) = TFun

