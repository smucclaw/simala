module Simala.Eval.Monad where

import Base
import qualified Base.Map as Map
import Simala.Eval.Type
import Simala.Expr.Type
import Util.RevList

renderEvalTrace :: EvalTrace -> String
renderEvalTrace = go 1
  where
    go lvl (Trace e subs v) = line lvl '>' (render e) <> concatMap (go (lvl + 1)) subs <> line lvl '<' (render v)
    line lvl c msg = replicate (lvl * 3) c <> " " <> msg <> "\n"

buildEvalTrace :: [EvalAction] -> EvalTrace
buildEvalTrace = go []
  where
    go :: [EvalFrame] -> [EvalAction] -> EvalTrace
    go fs                  (Enter e : actions) = go (Frame e emptyRevList : fs) actions
    go (Frame e subs : fs) (Exit v  : actions) =
      let
        t = Trace e (unRevList subs) v
      in
        case fs of
          []                     -> t -- TODO: strictly speaking, we should check if actions is empty
          (Frame e' subs' : fs') -> go (Frame e' (pushRevList t  subs') : fs') actions
    go _ _ = error "illegal eval action sequence"

-- | Second environment wins over first.
extendEnv :: Env -> Env -> Env
extendEnv = flip Map.union

arityError :: Int -> Int -> Eval a
arityError ae ao = throwError (ArityError ae ao)

typeError :: ValTy -> ValTy -> Eval a
typeError tye tyo = throwError (TypeError tye tyo)

scopeError :: Name -> Eval a
scopeError n = throwError (ScopeError n)

divByZero :: Eval a
divByZero = throwError DivByZero

emptyListError :: Eval a
emptyListError = throwError EmptyListError

look :: Name -> Eval Val -- fails if variable not in scope
look n = do
  env <- getEnv
  case Map.lookup n env of
    Nothing -> scopeError n
    Just v  -> pure v

expectBool :: Val -> Eval Bool -- fails if type-incorrect
expectBool (VBool b) = pure b
expectBool v         = typeError TBool (valTy v)

expectInt :: Val -> Eval Int -- fails if type-incorrect
expectInt (VInt i) = pure i
expectInt v        = typeError TInt (valTy v)

expectFunction :: Val -> Eval Closure
expectFunction (VClosure c) = pure c
expectFunction v            = typeError TFun (valTy v)

expectList :: Val -> Eval [Val]
expectList (VList vs) = pure vs
expectList v          = typeError TList (valTy v)

expectArity3 :: [a] -> Eval (a, a, a) -- fails if wrong length
expectArity3 [e1, e2, e3] = pure (e1, e2, e3)
expectArity3 es           = arityError 3 (length es)

expectArity2 :: [a] -> Eval (a, a) -- fails if wrong length
expectArity2 [e1, e2] = pure (e1, e2)
expectArity2 es       = arityError 2 (length es)

expectArity1 :: [a] -> Eval a -- fails if wrong length
expectArity1 [e] = pure e
expectArity1 es  = arityError 1 (length es)

expectNonZero :: Int -> Eval ()
expectNonZero 0 = divByZero
expectNonZero _ = pure ()

expectNonEmpty :: [Expr] -> Eval () -- fails if empty
expectNonEmpty [] = emptyListError
expectNonEmpty _  = pure ()

expectEmpty :: [Expr] -> Eval () -- fails if not empty
expectEmpty [] = pure ()
expectEmpty es = arityError 0 (length es)

crash :: Eval a -- abort program evaluation
crash = throwError Crash

pushEvalAction :: EvalAction -> Eval ()
pushEvalAction action =
  modifying #actions (pushRevList action)

enter :: Expr -> Eval ()
enter = pushEvalAction . Enter

exit :: Val -> Eval ()
exit = pushEvalAction . Exit

getEnv :: Eval Env
getEnv = use #env

withEnv :: Env -> Eval a -> Eval a
withEnv env m = do
  savedEnv <- getEnv
  assign #env env
  r <- m
  assign #env savedEnv
  pure r

runEval :: Eval a -> Either EvalError EvalTrace
runEval (MkEval m) =
  buildEvalTrace . unRevList . (.actions) . snd <$> m (MkEvalState Map.empty emptyRevList)
