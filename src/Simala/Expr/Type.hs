module Simala.Expr.Type where

import Base
import qualified Base.Map as Map

type Name = Text

-- | Declarations are used in the repl, in declaration files,
-- and in let expressions.
--
-- Mutually recursive operations are not directly possible right
-- now and have to be simulated via records.
--
data Decl =
    NonRec Transparency Name Expr
  | Rec    Transparency Name Expr
  | Eval   Expr
  deriving stock Show

-- | Repl instructions.
--
data Instruction =
    Declare Decl
  | ReplCommand ReplCommand
  | Noop
  deriving stock Show

data TraceMode =
    TraceOff
  | TraceResults
  | TraceFull
  deriving stock Show

data ReplCommand =
    SetTrace TraceMode
  | Help
  | Quit
  deriving stock Show

data Expr =
    Builtin    Builtin [Expr]              -- built-ins; the currently decide their own eval strategy, so can be used for control flow
  | Var        Name
  | Atom       Name                        -- for simulating enumeration types
  | Lit        Lit
  | Cons       Expr Expr                   -- should be a built-in?
  | List       [Expr]                      -- construct a list
  | App        Expr [Expr]                 -- function application
  | Record     (Row Expr)                  -- record construction
  | Project    Expr Name                   -- record projection
  | Fun        Transparency [Name] Expr    -- anonymous function
  | Let        Decl Expr                   -- possibly recursive let-binding
  | Undefined                              -- unclear
  deriving stock Show

type Row a = [(Name, a)]

data Transparency =
    Opaque
  | Transparent
  deriving stock (Eq, Ord, Show)

data Lit =
    IntLit     Int
  | BoolLit    Bool           -- could also be done using two nullary built-ins
  deriving stock Show

-- | Builtin operations.
data Builtin =
    Minus      -- ^ subtraction, arity 2
  | Divide     -- ^ (integer) division, arity 2
  | Modulo     -- ^ (integer) remainder, arity 2
  | Sum        -- ^ sum, flexible arity
  | Product    -- ^ product, flexible arity
  | Maximum    -- ^ maximum, arity > 0
  | Minimum    -- ^ minimum, arity > 0
  | Not        -- ^ logical negation, arity 1
  | Lt         -- ^ less-than, arity 2
  | Le         -- ^ less-than-or-equal, arity 2
  | Gt         -- ^ greater-than, arity 2
  | Ge         -- ^ greater-than-or-equal, arity 2
  | Eq         -- ^ equality (of Booleans, numbers or atoms), arity 2
  | HEq        -- ^ heterogeneous equality, arity 2 (will never crash, but return false on unsuported or different types)
  | Ne         -- ^ inequality (of Booleans, numbers or atoms), arity 2
  | And        -- ^ logical and, flexible arity
  | Or         -- ^ logical or, flexible arity
  | IfThenElse -- ^ if-then-else, arity 3
  | Foldl      -- ^ left fold, arity 3
  | Foldr      -- ^ right fold, arity 3
  | Case       -- ^ case on lists, arity 3
  | Merge      -- ^ Merge two records, arity 2
  deriving stock Show

-- | A value.
--
data Val =
    VInt  Int          -- ^ integer value
  | VBool Bool         -- ^ boolean value
  | VList [Val]        -- ^ fully evaluated list
  | VRecord (Row Val)  -- ^ fully evaluated record
  | VClosure Closure   -- ^ closure / suspended function
  | VAtom Name         -- ^ an atom is just a name that is not interpreted further
  | VBlackhole         -- ^ special value used in the evaluation of recursive lets
  deriving stock Show

-- | Types of values. As Simala is currently a dynamically
-- typed language, this is not at all precise. In particular,
-- we do not care about the contents of a list, or the fields
-- of a record.
--
data ValTy =
    TInt
  | TBool
  | TList
  | TRecord
  | TFun
  | TAtom
  | TBlackhole
  deriving stock Show

-- | A closure carries the following info:
--
-- * a transparency level; opaque means that if the
--   closure is applied, we don't want to see any inner
--   reduction steps in the trace and go to the result
--   immediately;
--
-- * a list of names the closure is "blocked" on;
--   the closure environment needs to be extended with
--   bindings for these so that the closure can be
--   applied; note that in principle, the list of names
--   can be empty, to denote a delayed computation that
--   can be run without any additional inputs being
--   required;
--
-- * an expression which is the body of the closure;
--
-- * an environment capturing potential free variables
--   in the body of the closure.
--
data Closure =
  MkClosure Transparency [Name] Expr Env
  deriving stock Show

-- | An environment maps names to values.
--
type Env = Map Name Val

-- | Type of a value.
valTy :: Val -> ValTy
valTy (VInt _)     = TInt
valTy (VBool _)    = TBool
valTy (VList _)    = TList
valTy (VRecord _)  = TRecord
valTy (VClosure _) = TFun
valTy (VAtom _)    = TAtom
valTy VBlackhole   = TBlackhole

-- | Attach transparency to a value. Currently, only
-- closured have transparency information; on all other
-- value types this is a noop.
--
attachTransparency :: Transparency -> Val -> Val
attachTransparency t (VClosure (MkClosure _ ns e env)) =
  VClosure (MkClosure t ns e env)
attachTransparency _ v = v

-- | An empty environment.
emptyEnv :: Env
emptyEnv = Map.empty

-- | Second environment wins over first.
extendEnv :: Env -> Env -> Env
extendEnv = flip Map.union

-- | An environment with a single binding.
singletonEnv :: Name -> Val -> Env
singletonEnv n v = Map.singleton n v

-- | Look up a name in an environment.
lookupInEnv :: Name -> Env -> Maybe Val
lookupInEnv = Map.lookup

-- | Helper function to create an if-then-else construct.
mkIfThenElse :: Expr -> Expr -> Expr -> Expr
mkIfThenElse c t e = Builtin IfThenElse [c, t, e]

-- | Helper function to create a nested let expression.
mkLet :: [Decl] -> Expr -> Expr
mkLet ds e = foldr Let e ds
