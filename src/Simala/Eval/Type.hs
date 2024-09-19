module Simala.Eval.Type where

import Base
import Simala.Expr.Type
import Util.RevList

-- * Abstract syntax

data EvalAction =
    Enter (Maybe Name) Expr
  | Exit Val
  | Exception EvalError
  deriving stock Show

data EvalTrace =
  Trace (Maybe Name) Expr [EvalTrace] (Either EvalError Val)
  deriving stock Show

data EvalState =
  MkEvalState
    { env          :: !Env
    , actions      :: RevList EvalAction
    , traces       :: RevList (Either EvalError Val, EvalTrace)
    , transparency :: !Transparency
    }
  deriving stock (Generic, Show)

data EvalFrame =
  Frame (Maybe Name) Expr (RevList EvalTrace)

type Eval :: Type -> Type
newtype Eval a =
  MkEval (EvalState -> (Either EvalError a, EvalState))
  deriving
    (Functor, Applicative, Monad, MonadState EvalState, MonadError EvalError)
    via ExceptT EvalError (StateT EvalState Identity)

data EvalError =
    ArityError Int Int -- expected, observed
  | TypeError [ValTy] ValTy -- expected one of, observed
  | ScopeError Name
  | DivByZero
  | EmptyListError
  | RecordProjectionError Name
  | Crash
  deriving stock Show
