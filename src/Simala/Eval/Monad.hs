module Simala.Eval.Monad where

import Base
import qualified Base.Text as Text
import Simala.Eval.Type
import Simala.Expr.Render
import Simala.Expr.Type
import Util.RevList

import Data.Bifunctor

renderEvalTrace :: EvalTrace -> Text
renderEvalTrace = go 1
  where
    go lvl (Trace e subs v) = line lvl ">" (render e) <> Text.concat (map (go (lvl + 1)) subs) <> renderResult lvl v -- line lvl '<' (render v)
    line lvl c msg = Text.replicate (lvl * 3) c <> " " <> msg <> "\n"
    renderResult lvl (Right x) = line lvl "<" (render x)
    renderResult lvl (Left x)  = line lvl "*" (render x)

buildEvalTrace :: [EvalAction] -> EvalTrace
buildEvalTrace = go []
  where
    go :: [EvalFrame] -> [EvalAction] -> EvalTrace
    go fs                  (Enter e : actions) = go (Frame e emptyRevList : fs) actions
    go (Frame e subs : fs) (Exit v  : actions) =
      let
        t = Trace e (unRevList subs) (Right v)
      in
        case fs of
          []                     -> t -- TODO: strictly speaking, we should check if actions is empty
          (Frame e' subs' : fs') -> go (Frame e' (pushRevList t subs') : fs') actions
    go (Frame e subs : fs) actions@(Exception exc : _) =
      let
        t = Trace e (unRevList subs) (Left exc)
      in
        case fs of
          []                     -> t
          (Frame e' subs' : fs') -> go (Frame e' (pushRevList t subs') : fs') actions
    go _ _ = error "illegal eval action sequence"

simplifyEvalTrace' :: EvalTrace -> Maybe EvalTrace
simplifyEvalTrace' (Trace e subs v) =
  let
    subs' = mapMaybe simplifyEvalTrace' subs
    t' = Trace e subs' v
  in
    case t' of
      Trace (Lit _)    [] _ -> Nothing
      Trace (List _)   [] _ -> Nothing
      Trace (Record _) [] _ -> Nothing
      Trace (Atom _)   [] _ -> Nothing
      _                     -> Just t'

simplifyEvalTrace :: EvalTrace -> EvalTrace
simplifyEvalTrace t@(Trace e subs v) =
  case simplifyEvalTrace' t of
    Nothing -> Trace e (mapMaybe simplifyEvalTrace' subs) v
    Just t' -> t'

arityError :: Int -> Int -> Eval a
arityError ae ao = raise (ArityError ae ao)

typeError :: ValTy -> ValTy -> Eval a
typeError tye tyo = raise (TypeError tye tyo)

scopeError :: Name -> Eval a
scopeError n = raise (ScopeError n)

divByZero :: Eval a
divByZero = raise DivByZero

emptyListError :: Eval a
emptyListError = raise EmptyListError

recordProjectionError :: Name -> Eval a
recordProjectionError n = raise (RecordProjectionError n)

getTransparency :: Eval Transparency
getTransparency = use #transparency

-- | Should not be used directly; use 'withTransparency'.
setTransparency :: Transparency -> Eval ()
setTransparency t = assign' #transparency t

-- | We can currently only lower the transparency.
withTransparency :: Transparency -> Eval a -> Eval a
withTransparency t' m = do
  t <- getTransparency
  let t'' = min t t'
  setTransparency t''
  x <- m
  setTransparency t
  pure x

look :: Name -> Eval Val -- fails if variable not in scope
look n = do
  env <- getEnv
  case lookupInEnv n env of
    Nothing -> scopeError n
    Just v  -> pure v

expectBool :: Val -> Eval Bool -- fails if type-incorrect
expectBool (VBool b) = pure b
expectBool v         = typeError TBool (valTy v)

expectInt :: Val -> Eval Int -- fails if type-incorrect
expectInt (VInt i) = pure i
expectInt v        = typeError TInt (valTy v)

expectAtom :: Val -> Eval Name -- fails if type-incorrect
expectAtom (VAtom x) = pure x
expectAtom v         = typeError TAtom (valTy v)

expectFunction :: Val -> Eval Closure
expectFunction (VClosure c) = pure c
expectFunction v            = typeError TFun (valTy v)

expectList :: Val -> Eval [Val]
expectList (VList vs) = pure vs
expectList v          = typeError TList (valTy v)

expectRecord :: Val -> Eval (Row Val)
expectRecord (VRecord r) = pure r
expectRecord v           = typeError TRecord (valTy v)

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

raise :: EvalError -> Eval a
raise e = do
  pushEvalAction (Exception e)
  throwError e

crash :: Eval a -- abort program evaluation
crash = raise Crash

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

initialEvalState :: EvalState
initialEvalState =
  MkEvalState emptyEnv emptyRevList Transparent

runEval :: Eval a -> (Either EvalError a, EvalTrace)
runEval (MkEval m) =
  second
    (simplifyEvalTrace . buildEvalTrace . unRevList . (.actions))
    (m initialEvalState)

