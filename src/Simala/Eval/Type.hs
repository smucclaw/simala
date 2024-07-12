module Simala.Eval.Type where

import Base
import Simala.Expr.Type
import Util.RevList

-- * Abstract syntax

data EvalAction =
    Enter Expr
  | Exit Val
  | Exception EvalError
  deriving stock Show

data EvalTrace =
  Trace Expr [EvalTrace] (Either EvalError Val)
  deriving stock Show

data EvalState =
  MkEvalState
    { env          :: !Env
    , actions      :: RevList EvalAction
    , transparency :: !Transparency
    }
  deriving stock (Generic, Show)

data EvalFrame =
  Frame Expr (RevList EvalTrace)

type Eval :: Type -> Type
newtype Eval a =
  MkEval (EvalState -> (Either EvalError a, EvalState))
  deriving
    (Functor, Applicative, Monad, MonadState EvalState, MonadError EvalError)
    via ExceptT EvalError (StateT EvalState Identity)

data EvalError =
    ArityError Int Int -- expected, observed
  | TypeError ValTy ValTy -- expected, observed
  | ScopeError Name
  | DivByZero
  | EmptyListError
  | RecordProjectionError Name
  | Crash
  deriving stock Show
