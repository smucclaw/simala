module Simala.Eval.Type where

type Eval :: Type -> Type
newtype Eval a =
  MkEval (EvalState -> Either EvalError (a, EvalState))
  deriving
    (Functor, Applicative, Monad, MonadState EvalState, MonadError EvalError)
    via StateT EvalState (ExceptT EvalError Identity)

