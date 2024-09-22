module Simala.Expr.Evaluator where

import Base
import qualified Base.Map as Map
import qualified Base.Text as Text
import Simala.Eval.Monad
import Simala.Eval.Type
import Simala.Expr.Render
import Simala.Expr.Type
import Util.RevList

-- | Evaluate an expression. Produces a trace if
-- the current transparency level warrants it.
--
eval :: Expr -> Eval Val
eval e = do
  t <- getTransparency
  when (t == Transparent) (enter Nothing e)
  v <- eval' e
  when (t == Transparent) (exit v)
  pure v

-- | Evaluate an expression, but change the
-- transparency level only for the inner part.
--
evalWithTransparency :: Transparency -> Name -> Expr -> Eval Val
evalWithTransparency t' n e = do
  t <- getTransparency
  when (t == Transparent) (enter (Just n) e)
  v <- withTransparency t' (eval' e)
  when (t == Transparent) (exit v)
  pure v

-- | Evaluate an expression. Produces a trace
-- only for recursive calls.
--
-- This is intended to be called only by 'eval'.
--
eval' :: Expr -> Eval Val
eval' (Builtin b exprs)   = evalBuiltin b exprs
eval' (Var x)             = look x
eval' (Atom x)            = pure (VAtom x)
eval' Undefined           = crash
eval' (Lit l)             = evalLit l
eval' (Record r)          = do
  vs <- traverse (\ (x, e) -> (x,) <$> eval e) r
  pure (VRecord vs)
eval' (Project e x)       = do
  v <- eval e
  r <- expectRecord v
  case lookup x r of
    Nothing -> recordProjectionError x
    Just vx -> pure vx
eval' (App f args)        = do
  v <- eval f
  c <- expectFunction v
  evalClosure c args
eval' (Fun t ns body)     = do
  env <- getEnv
  pure (VClosure (MkClosure t ns body env))
eval' (Let d e)     = do
  env' <- evalDecl d
  env <- getEnv
  withEnv (extendEnv env env') (eval e)

evalDecl :: Decl -> Eval Env
evalDecl (NonRec t n e) = do
  v <- evalWithTransparency t n e
  let v' = attachTransparency t v
  pure (singletonEnv n v')
evalDecl (Rec t n e) = do
  let env' = singletonEnv n VBlackhole
  env <- getEnv
  v <- withEnv (extendEnv env env') (evalWithTransparency t n e)
  let
    env'' = singletonEnv n v'
    v' =
      case v of
        -- if we get a closure, we actually replace the blackhole in the closure environment
        VClosure (MkClosure _ ns body envc) -> VClosure (MkClosure t ns body (extendEnv envc env''))
        _ -> v -- TODO: we could be doing something better for lists and in particular records, but
               -- currently we just fall back to non-recursive let
  pure env''
evalDecl (Eval e) = do
  env <- getEnv
  case runEval (withEnv env (eval e)) of
    (r, t) -> do
      modifying' #traces (pushRevList (r, t))
  pure env

evalDecls :: [Decl] -> Eval ()
evalDecls = traverse_ $ \ d -> do
  env' <- evalDecl d
  env <- getEnv
  setEnv (extendEnv env env')

evalClosure :: Closure -> [Expr] -> Eval Val
evalClosure (MkClosure t argNames body env) args = do
  env' <- bind argNames args
  withEnv (extendEnv env env') (withTransparency t (eval body))

evalClosureVal :: Closure -> [Val] -> Eval Val
evalClosureVal (MkClosure t argNames body env) args = do
  env' <- bindVal argNames args
  withEnv (extendEnv env env') (withTransparency t (eval body))

bind :: [Name] -> [Expr] -> Eval Env
bind ns0 es0 = go Map.empty ns0 es0
  where
    go !acc []       []       = pure acc
    go !acc (n : ns) (e : es) = do
      v <- eval e
      go (Map.insert n v acc) ns es
    go _    _        _        = arityError (length ns0) (length es0)

bindVal :: [Name] -> [Val] -> Eval Env
bindVal ns0 vs0 = go Map.empty ns0 vs0
  where
    go !acc []       []       = pure acc
    go !acc (n : ns) (v : vs) = do
      go (Map.insert n v acc) ns vs
    go _    _        _        = arityError (length ns0) (length vs0)

evalLit :: Lit -> Eval Val
evalLit (IntLit i)    = pure (VInt i)
evalLit (BoolLit b)   = pure (VBool b)
evalLit (StringLit s) = pure (VString s)
evalLit (FracLit f)   = pure (VFrac f)

expect1Int :: [Expr] -> Eval Int
expect1Int exprs = do
  e <- expectArity1 exprs
  (eval >=> expectInt) e

expect1Frac :: [Expr] -> Eval Double
expect1Frac exprs = do
  e <- expectArity1 exprs
  (eval >=> expectFrac) e

expect1Bool :: [Expr] -> Eval Bool
expect1Bool exprs = do
  e <- expectArity1 exprs
  (eval >=> expectBool) e

expect1String :: [Expr] -> Eval Text
expect1String exprs = do
  e <- expectArity1 exprs
  (eval >=> expectString) e

expect2Strings :: [Expr] -> Eval (Text, Text)
expect2Strings exprs = do
  (e1, e2) <- expectArity2 exprs
  s1 <- (eval >=> expectString) e1
  s2 <- (eval >=> expectString) e2
  pure (s1, s2)

expect2Ints :: [Expr] -> Eval (Int, Int)
expect2Ints exprs = do
  (e1, e2) <- expectArity2 exprs
  i1 <- (eval >=> expectInt) e1
  i2 <- (eval >=> expectInt) e2
  pure (i1, i2)

expect2Nums :: [Expr] -> (Int -> Int -> Eval a) -> (Double -> Double -> Eval a) -> Eval a
expect2Nums exprs ki kf = do
  (e1, e2) <- expectArity2 exprs
  v1 <- eval e1
  case v1 of
    VInt i1 -> do
      i2 <- (eval >=> expectInt) e2
      ki i1 i2
    VFrac f1 -> do
      f2 <- (eval >=> expectFrac) e2
      kf f1 f2
    v -> typeError [TInt, TFrac] (valTy v)

expect2NumsOrStrings :: [Expr] -> (Int -> Int -> Eval a) -> (Double -> Double -> Eval a) -> (Text -> Text -> Eval a) -> Eval a
expect2NumsOrStrings exprs ki kf ks = do
  (e1, e2) <- expectArity2 exprs
  v1 <- eval e1
  case v1 of
    VInt i1 -> do
      i2 <- (eval >=> expectInt) e2
      ki i1 i2
    VFrac f1 -> do
      f2 <- (eval >=> expectFrac) e2
      kf f1 f2
    VString s1 -> do
      s2 <- (eval >=> expectString) e2
      ks s1 s2
    v -> typeError [TInt, TFrac, TString] (valTy v)

expect2Bools :: [Expr] -> Eval (Bool, Bool)
expect2Bools exprs = do
  (e1, e2) <- expectArity2 exprs
  b1 <- (eval >=> expectBool) e1
  b2 <- (eval >=> expectBool) e2
  pure (b1, b2)

expect2NumsOrStringsOrBoolsOrAtoms :: [Expr] -> (Int -> Int -> Eval a) -> (Double -> Double -> Eval a) -> (Text -> Text -> Eval a) -> (Bool -> Bool -> Eval a) -> (Atom -> Atom -> Eval a) -> Eval a
expect2NumsOrStringsOrBoolsOrAtoms exprs ki kf ks kb ka = do
  (e1, e2) <- expectArity2 exprs
  v1 <- eval e1
  case v1 of
    VBool b1 -> do
      b2 <- (eval >=> expectBool) e2
      kb b1 b2
    VInt i1 -> do
      i2 <- (eval >=> expectInt) e2
      ki i1 i2
    VFrac f1 -> do
      f2 <- (eval >=> expectFrac) e2
      kf f1 f2
    VString s1 -> do
      s2 <- (eval >=> expectString) e2
      ks s1 s2
    VAtom x1 -> do
      x2 <- (eval >=> expectAtom) e2
      ka x1 x2
    v -> typeError [TBool, TInt, TFrac, TAtom] (valTy v)

-- TODO: weird pitfall; we have to make a choice if the number of args is empty!
expectNums :: [Expr] -> ([Int] -> Eval a) -> ([Double] -> Eval a) -> Eval a
expectNums [] ki _kf = ki []
expectNums (e : es) ki kf = do
  v <- eval e
  case v of
    VInt i -> traverse (eval >=> expectInt) es >>= \ is -> ki (i : is)
    VFrac f -> traverse (eval >=> expectFrac) es >>= \ fs -> kf (f : fs)
    _ -> typeError [TInt, TFrac] (valTy v)

evalBuiltin :: Builtin -> [Expr] -> Eval Val
evalBuiltin Minus exprs =
  expect2Nums exprs
    (\ i1 i2 -> pure (VInt (i1 - i2)))
    (\ f1 f2 -> pure (VFrac (f1 - f2)))
evalBuiltin Divide exprs =
  expect2Nums exprs
    (\ i1 i2 -> expectNonZero i2 >> pure (VInt (i1 `div` i2)))
    (\ f1 f2 -> pure (VFrac (f1 / f2)))
evalBuiltin Modulo exprs = do
  (i1, i2) <- expect2Ints exprs
  expectNonZero i2
  pure (VInt (i1 `mod` i2))
evalBuiltin Sum exprs = do
  expectNums exprs
    (\ is -> pure (VInt (sum is)))
    (\ fs -> pure (VFrac (sum fs)))
evalBuiltin Product exprs = do
  expectNums exprs
    (\ is -> pure (VInt (product is)))
    (\ fs -> pure (VFrac (product fs)))
evalBuiltin Maximum exprs = do
  expectNonEmpty exprs
  expectNums exprs -- TODO: support strings
    (\ is -> pure (VInt (maximum is)))
    (\ fs -> pure (VFrac (maximum fs)))
evalBuiltin Minimum exprs = do
  expectNonEmpty exprs
  expectNums exprs -- TODO: support strings
    (\ is -> pure (VInt (minimum is)))
    (\ fs -> pure (VFrac (minimum fs)))
evalBuiltin Not exprs = do
  b <- expect1Bool exprs
  pure (VBool (not b))
evalBuiltin Lt exprs = do
  expect2NumsOrStrings exprs
    (\ i1 i2 -> pure (VBool (i1 < i2)))
    (\ f1 f2 -> pure (VBool (f1 < f2)))
    (\ s1 s2 -> pure (VBool (s1 < s2)))
evalBuiltin Le exprs = do
  expect2NumsOrStrings exprs
    (\ i1 i2 -> pure (VBool (i1 <= i2)))
    (\ f1 f2 -> pure (VBool (f1 <= f2)))
    (\ s1 s2 -> pure (VBool (s1 <= s2)))
evalBuiltin Gt exprs = do
  expect2NumsOrStrings exprs
    (\ i1 i2 -> pure (VBool (i1 > i2)))
    (\ f1 f2 -> pure (VBool (f1 > f2)))
    (\ s1 s2 -> pure (VBool (s1 > s2)))
evalBuiltin Ge exprs = do
  expect2NumsOrStrings exprs
    (\ i1 i2 -> pure (VBool (i1 >= i2)))
    (\ f1 f2 -> pure (VBool (f1 >= f2)))
    (\ s1 s2 -> pure (VBool (s1 >= s2)))
evalBuiltin Eq exprs = do
  expect2NumsOrStringsOrBoolsOrAtoms exprs
    (\ i1 i2 -> pure (VBool (i1 == i2)))
    (\ f1 f2 -> pure (VBool (f1 == f2)))
    (\ s1 s2 -> pure (VBool (s1 == s2)))
    (\ b1 b2 -> pure (VBool (b1 == b2)))
    (\ x1 x2 -> pure (VBool (x1 == x2)))
evalBuiltin HEq exprs = do
  (e1, e2) <- expectArity2 exprs
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VInt i1, VInt i2) -> pure (VBool (i1 == i2))
    (VFrac f1, VFrac f2) -> pure (VBool (f1 == f2))
    (VString s1, VString s2) -> pure (VBool (s1 == s2))
    (VBool b1, VBool b2) -> pure (VBool (b1 == b2))
    (VAtom a1, VAtom a2) -> pure (VBool (a1 == a2))
    (_, _) -> pure (VBool False)
evalBuiltin Ne exprs = do
  expect2NumsOrStringsOrBoolsOrAtoms exprs
    (\ i1 i2 -> pure (VBool (i1 /= i2)))
    (\ f1 f2 -> pure (VBool (f1 /= f2)))
    (\ s1 s2 -> pure (VBool (s1 /= s2)))
    (\ b1 b2 -> pure (VBool (b1 /= b2)))
    (\ x1 x2 -> pure (VBool (x1 /= x2)))
evalBuiltin And [] =
  pure (VBool True)
evalBuiltin And (e : es) = do
  v <- (eval >=> expectBool) e
  case v of
    False -> pure (VBool False)
    True  -> evalBuiltin And es
evalBuiltin Or [] =
  pure (VBool False)
evalBuiltin Or (e : es) = do
  v <- (eval >=> expectBool) e
  case v of
    False -> evalBuiltin Or es
    True  -> pure (VBool True)
evalBuiltin IfThenElse es = do
  (c, t, e) <- expectArity3 es
  v <- eval c
  b <- expectBool v
  if b then eval t else eval e
evalBuiltin Foldl es = do
  (e1, e2, e3) <- expectArity3 es
  vs <- (eval >=> expectList) e3
  vini <- eval e2
  c <- (eval >=> expectFunction) e1
  let
    loop !acc list =
      case list of
        [] -> pure acc
        (w : ws) -> do
          acc' <- evalClosureVal c [acc, w]
          loop acc' ws
  loop vini vs
evalBuiltin Foldr es = do
  (e1, e2, e3) <- expectArity3 es
  vs <- (eval >=> expectList) e3
  vnil <- eval e2
  c <- (eval >=> expectFunction) e1
  let
    go []       = pure vnil
    go (w : ws) = do
      r <- go ws
      evalClosureVal c [w, r]
  go vs
evalBuiltin Case es = do
  (e1, e2, e3) <- expectArity3 es
  v3 <- (eval >=> expectList) e3
  case v3 of
    [] -> eval e1
    (v : vs) -> do
      c <- (eval >=> expectFunction) e2
      evalClosureVal c [v, VList vs]
evalBuiltin Merge es = do
  (e1, e2) <- expectArity2 es
  r1 <- (eval >=> expectRecord) e1
  r2 <- (eval >=> expectRecord) e2
  -- Right biased merging of records.
  -- We could be smarter in the case of nested records.
  let r = Map.toList $ Map.unionWith (\_ b -> b) (Map.fromList r1) (Map.fromList r2)
  pure $ VRecord r
evalBuiltin Floor es = do
  f <- expect1Frac es
  pure $ VInt (floor f)
evalBuiltin Ceiling es = do
  f <- expect1Frac es
  pure $ VInt (ceiling f)
evalBuiltin FromInt es = do
  i <- expect1Int es
  pure $ VFrac (fromIntegral i)
evalBuiltin Explode es = do
  s <- expect1String es
  pure $ VList (VString <$> Text.chunksOf 1 s)
evalBuiltin Append es = do
  (s1, s2) <- expect2Strings es
  pure $ VString (s1 <> s2)
evalBuiltin TypeOf es = do
  e <- expectArity1 es
  v <- eval e
  pure $ VAtom (MkAtom $ atomFor (valTy v))
evalBuiltin Cons es   = do
  (e1, e2) <- expectArity2 es
  v1 <- eval e1
  v2 <- eval e2
  vs <- expectList v2
  pure (VList (v1 : vs))
evalBuiltin List es   = do
  vs <- traverse eval es
  pure (VList vs)

atomFor :: ValTy -> Name
atomFor TInt       = "int"
atomFor TBool      = "bool"
atomFor TString    = "string"
atomFor TFrac      = "frac"
atomFor TList      = "list"
atomFor TRecord    = "record"
atomFor TFun       = "fun"
atomFor TAtom      = "atom"
atomFor TBlackhole = "blackhole"

doEvalDeclsTracing :: TraceMode -> Env -> [Decl] -> IO Env
doEvalDeclsTracing tracing env ds =
  case runEval' (setEnv env >> evalDecls ds >> getEnv) of
    (r, resultsAndTraces) -> do
      case tracing of
        TraceOff     -> do
          renderEvaluationResultsWith resultsAndTraces Nothing
        TraceResults -> do
          renderEvaluationResultsWith resultsAndTraces (Just renderResultsTrace)
        TraceFull    -> do
          renderEvaluationResultsWith resultsAndTraces (Just renderFullTrace)
      processFinalResult r
  where
    processFinalResult r =
      case r of
        Left err -> do
          print err
          pure env
        Right env' -> do
          pure env'

doEvalDeclTracing :: TraceMode -> Env -> Decl -> IO (Env -> Env)
doEvalDeclTracing tracing env d =
  case runEval' (withEnv env (evalDecl d)) of
    (r, resultsAndTraces) -> do
      case tracing of
        TraceOff     -> do
          renderEvaluationResultsWith resultsAndTraces Nothing
        TraceResults -> do
          renderEvaluationResultsWith resultsAndTraces (Just renderResultsTrace)
        TraceFull    -> do
          renderEvaluationResultsWith resultsAndTraces (Just renderFullTrace)
      processFinalResult r
  where
    processFinalResult r =
      case r of
        Left err -> do
          print err
          pure id
        Right env' -> do
          pure (\ x -> extendEnv x env')

renderEvaluationResultsWith :: [(Either EvalError Val, EvalTrace)] -> Maybe (EvalTrace -> Text) -> IO ()
renderEvaluationResultsWith xs Nothing = traverse_ (renderIntermediateResult . fst) xs
renderEvaluationResultsWith xs (Just f) = traverse_ (\(val, trace) -> do
  Text.putStr $ f trace
  renderIntermediateResult val
  ) xs

renderIntermediateResult :: Either EvalError Val -> IO ()
renderIntermediateResult r =
  case r of
    Left err -> print err
    Right x  -> Text.putStrLn (renderAsText x)
