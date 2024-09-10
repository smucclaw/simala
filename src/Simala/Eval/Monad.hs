module Simala.Eval.Monad where

import Base
import qualified Base.Text as Text
import Simala.Eval.Type
import Simala.Expr.Render
import Simala.Expr.Type
import Util.RevList

import Data.Bifunctor

renderFullTrace :: EvalTrace -> Text
renderFullTrace = go 1
  where
    go lvl (Trace (Just n) e subs v) = line lvl ">" (render n <> " = " <> render e) <> Text.concat (map (go (lvl + 1)) subs) <> renderResult lvl (Just n) v
    go lvl (Trace Nothing  e subs v) = line lvl ">" (render e) <> Text.concat (map (go (lvl + 1)) subs) <> renderResult lvl Nothing v -- line lvl '<' (render v)
    line lvl c msg = Text.replicate (lvl * 3) c <> " " <> msg <> "\n"
    renderResult :: Int -> Maybe Name -> Either EvalError Val -> Text
    renderResult lvl (Just n) (Right x) = line lvl "<" (render n <> " = " <> render x)
    renderResult lvl (Just n) (Left x)  = line lvl "*" (render n <> " aborted with " <> render x)
    renderResult lvl Nothing  (Right x) = line lvl "<" (render x)
    renderResult lvl Nothing  (Left x)  = line lvl "*" (render x)

renderResultsTrace :: EvalTrace -> Text
renderResultsTrace = go
  where
    go (Trace mn _ subs v) = Text.concat (map go subs) <> renderResult mn v
    renderResult :: Maybe Name -> Either EvalError Val -> Text
    renderResult (Just n) (Right x) = render n <> " = " <> render x <> "\n"
    renderResult (Just n) (Left x)  = render n <> " aborted with " <> render x <> "\n"
    renderResult Nothing  _         = ""

buildEvalTrace :: [EvalAction] -> EvalTrace
buildEvalTrace = go []
  where
    go :: [EvalFrame] -> [EvalAction] -> EvalTrace
    go fs                     (Enter mn e : actions) = go (Frame mn e emptyRevList : fs) actions
    go (Frame mn e subs : fs) (Exit v     : actions) =
      let
        t = Trace mn e (unRevList subs) (Right v)
      in
        case fs of
          []                         -> t -- TODO: strictly speaking, we should check if actions is empty
          (Frame mn' e' subs' : fs') -> go (Frame mn' e' (pushRevList t subs') : fs') actions
    go (Frame mn e subs : fs) actions@(Exception exc : _) =
      let
        t = Trace mn e (unRevList subs) (Left exc)
      in
        case fs of
          []                     -> t
          (Frame mn' e' subs' : fs') -> go (Frame mn' e' (pushRevList t subs') : fs') actions
    go _ _ = error "illegal eval action sequence"

simplifyEvalTrace' :: EvalTrace -> Maybe EvalTrace
simplifyEvalTrace' (Trace mn e subs v) =
  let
    subs' = mapMaybe simplifyEvalTrace' subs
    t' = Trace mn e subs' v
  in
    -- NOTE: We do not simply subtraces with explicit names, because
    -- we assume they're supposed to be seen in the trace.
    case t' of
      Trace Nothing (Lit _)    [] _ -> Nothing
      Trace Nothing (List _)   [] _ -> Nothing
      Trace Nothing (Record _) [] _ -> Nothing
      Trace Nothing (Atom _)   [] _ -> Nothing
      _                             -> Just t'

simplifyEvalTrace :: EvalTrace -> EvalTrace
simplifyEvalTrace t@(Trace mn e subs v) =
  case simplifyEvalTrace' t of
    Nothing -> Trace mn e (mapMaybe simplifyEvalTrace' subs) v
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

enter :: Maybe Name -> Expr -> Eval ()
enter mn = pushEvalAction . Enter mn

exit :: Val -> Eval ()
exit = pushEvalAction . Exit

getEnv :: Eval Env
getEnv = use #env

setEnv :: Env -> Eval ()
setEnv = assign #env

withEnv :: Env -> Eval a -> Eval a
withEnv env m = do
  savedEnv <- getEnv
  setEnv env
  r <- m
  setEnv savedEnv
  pure r

initialEvalState :: EvalState
initialEvalState =
  MkEvalState emptyEnv emptyRevList emptyRevList Transparent

runEval :: Eval a -> (Either EvalError a, EvalTrace)
runEval (MkEval m) =
  second
    (simplifyEvalTrace . buildEvalTrace . unRevList . (.actions))
    (m initialEvalState)

runEval' :: Eval a -> (Either EvalError a, [(Either EvalError Val, EvalTrace)])
runEval' (MkEval m) =
    (\( a, s ) ->
      ( a
      , unRevList $ s.traces
      )
    )
    (m initialEvalState)
