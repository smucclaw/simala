module Simala.Expr.Type where

import Base
import qualified Base.Map as Map
import Simala.Expr.Metadata

type Name = Text

data Variable = Variable
  { meta :: Meta
  , name :: Name
  }
  deriving stock Show

-- | Declarations are used in the repl, in declaration files,
-- and in let expressions.
--
-- Mutually recursive operations are not directly possible right
-- now and have to be simulated via records.
--
data Decl =
    NonRec Meta Transparency Variable Expr
  | Rec    Meta Transparency Variable Expr
  | Eval   Meta Expr
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
    Builtin    Meta Builtin [Expr]              -- built-ins; they currently decide their own eval strategy, so can be used for control flow
  | Var        Meta Variable
  | Atom       Meta Variable                    -- for simulating enumeration types
  | Lit        Meta Lit
  | App        Meta Expr [Expr]                 -- function application (not a built-in because it's so fundamental)
  | Record     Meta (Rows Expr)                  -- record construction
  | Project    Meta Expr Variable                   -- record projection
  | Fun        Meta Transparency [Variable] Expr    -- anonymous function
  | Let        Meta Decl Expr                   -- possibly recursive let-binding
  | Undefined  Meta                             -- unclear
  | Parens     Meta Expr
  deriving stock Show

data Rows a = Rows Meta [Row a]
  -- Meta contains `,`
  deriving stock Show
  deriving (Functor, Foldable, Traversable)

data Row a = Row
  { meta :: Meta
  , var :: Variable
  , payload :: a
  }
  -- Meta contains `=`
  deriving stock Show
  deriving (Functor, Foldable, Traversable)

data Transparency =
    Opaque
  | Transparent
  deriving stock (Eq, Ord, Show)

data Lit =
    IntLit     Int
  | FracLit    Double         -- fractional literal, TODO: not sure if we want IEEE semantics
  | BoolLit    Bool           -- could also be done using two nullary built-ins
  | StringLit  Text
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
  | Floor      -- ^ Round down a fractional to the nearest integer, arity 1
  | Ceiling    -- ^ Round up a fractional to the nearest integer, arity 1
  | FromInt    -- ^ Convert an integer to a fractional, arity 1
  | Explode    -- ^ Explode a string into a list of single-character strings, arity 1
  | Append     -- ^ Append two strings, arity 2
  | TypeOf     -- ^ run-time type checking, arity 1, returns an atom indicating the type
  | Cons       -- ^ cons for lists, arity 2
  | List       -- ^ construct a list, flexible arity
  deriving stock Show

-- | A value.
--
data Val =
    VInt  Int          -- ^ integer value
  | VBool Bool         -- ^ boolean value
  | VString Text       -- ^ string value
  | VFrac Double       -- ^ fractional value
  | VList [Val]        -- ^ fully evaluated list
  | VRecord (Rows Val)  -- ^ fully evaluated record
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
  | TString
  | TFrac
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
valTy (VString _)  = TString
valTy (VFrac _)    = TFrac
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

-- | Extend an environment with a single binding.
addToEnv :: Name -> Val -> Env -> Env
addToEnv = Map.insert

-- | An environment with a single binding.
singletonEnv :: Name -> Val -> Env
singletonEnv n v = Map.singleton n v

-- | Look up a name in an environment.
lookupInEnv :: Name -> Env -> Maybe Val
lookupInEnv = Map.lookup

-- | Helper function to create an if-then-else construct.
mkIfThenElse :: Meta -> Expr -> Expr -> Expr -> Expr
mkIfThenElse m c t e = Builtin m IfThenElse [c, t, e]

-- | Helper function to create a nested let expression.
mkLet :: Meta -> [Decl] -> Expr -> Expr
mkLet _m [] e =
  e
mkLet m (d:ds) e =
  Let m d (foldr (Let emptyMeta) e ds)

-- | Helper function to create a cons.
mkCons :: Meta -> Expr -> Expr -> Expr
mkCons m x xs = Builtin m Cons [x, xs]

-- | Helper function to create a list.
mkList :: Meta -> [Expr] -> Expr
mkList m xs = Builtin m List xs
