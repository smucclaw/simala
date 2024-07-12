module Simala.Expr.Evaluator where

import Base
import qualified Base.Map as Map
import qualified Base.Text as Text
import Simala.Eval.Monad
import Simala.Eval.Type
import Simala.Expr.Render
import Simala.Expr.Type

-- | Evaluate an expression. Produces a trace if
-- the current transparency level warrants it.
--
eval :: Expr -> Eval Val
eval e = do
  t <- getTransparency
  when (t == Transparent) (enter e)
  v <- eval' e
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
eval' (List ls)           = do
  vs <- traverse eval ls
  pure (VList vs)
eval' (Cons e1 e2)        = do
  v1 <- eval e1
  v2 <- eval e2
  vs <- expectList v2
  pure (VList (v1 : vs))
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
eval' (Let t n e1 e2)     = do
  env' <- evalDecl (NonRec t n e1)
  env <- getEnv
  withEnv (extendEnv env env') (eval e2)
eval' (Letrec t n e1 e2)  = do
  env'' <- evalDecl (Rec t n e1)
  env <- getEnv
  withEnv (extendEnv env env'') (eval e2)

evalDecl :: Decl -> Eval Env
evalDecl (NonRec t n e) = do
  v <- eval e
  let v' = attachTransparency t v
  pure (singletonEnv n v')
evalDecl (Rec t n e) = do
  let env' = singletonEnv n VBlackhole
  env <- getEnv
  v <- withEnv (extendEnv env env') (eval e)
  MkClosure _ ns body envc <- expectFunction v
  let env'' = singletonEnv n v'
      v' = VClosure (MkClosure t ns body (extendEnv envc env''))
  pure env''

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
evalLit (IntLit i)  = pure (VInt i)
evalLit (BoolLit b) = pure (VBool b)

expect1Bool :: [Expr] -> Eval Bool
expect1Bool exprs = do
  e <- expectArity1 exprs
  (eval >=> expectBool) e

expect2Ints :: [Expr] -> Eval (Int, Int)
expect2Ints exprs = do
  (e1, e2) <- expectArity2 exprs
  i1 <- (eval >=> expectInt) e1
  i2 <- (eval >=> expectInt) e2
  pure (i1, i2)

expect2Bools :: [Expr] -> Eval (Bool, Bool)
expect2Bools exprs = do
  (e1, e2) <- expectArity2 exprs
  b1 <- (eval >=> expectBool) e1
  b2 <- (eval >=> expectBool) e2
  pure (b1, b2)

expect2IntsOrBoolsOrAtoms :: [Expr] -> (Int -> Int -> Eval a) -> (Bool -> Bool -> Eval a) -> (Name -> Name -> Eval a) -> Eval a
expect2IntsOrBoolsOrAtoms exprs ki kb ka = do
  (e1, e2) <- expectArity2 exprs
  v1 <- eval e1
  case v1 of
    VBool b1 -> do
      b2 <- (eval >=> expectBool) e2
      kb b1 b2
    VInt i1 -> do
      i2 <- (eval >=> expectInt) e2
      ki i1 i2
    VAtom x1 -> do
      x2 <- (eval >=> expectAtom) e2
      ka x1 x2
    v -> typeError TInt (valTy v) -- TODO: Int or Bool expected

evalBuiltin :: Builtin -> [Expr] -> Eval Val
evalBuiltin Minus exprs = do
  (i1, i2) <- expect2Ints exprs
  pure (VInt (i1 - i2))
evalBuiltin Divide exprs = do
  (i1, i2) <- expect2Ints exprs
  expectNonZero i2
  pure (VInt (i1 `div` i2))
evalBuiltin Modulo exprs = do
  (i1, i2) <- expect2Ints exprs
  expectNonZero i2
  pure (VInt (i1 `mod` i2))
evalBuiltin Sum exprs = do
  is <- traverse (eval >=> expectInt) exprs
  pure (VInt (sum is))
evalBuiltin Product exprs = do
  is <- traverse (eval >=> expectInt) exprs
  pure (VInt (product is))
evalBuiltin Maximum exprs = do
  expectNonEmpty exprs
  is <- traverse (eval >=> expectInt) exprs
  pure (VInt (maximum is))
evalBuiltin Minimum exprs = do
  expectNonEmpty exprs
  is <- traverse (eval >=> expectInt) exprs
  pure (VInt (minimum is))
evalBuiltin Not exprs = do
  b <- expect1Bool exprs
  pure (VBool (not b))
evalBuiltin Lt exprs = do
  (i1, i2) <- expect2Ints exprs
  pure (VBool (i1 < i2))
evalBuiltin Le exprs = do
  (i1, i2) <- expect2Ints exprs
  pure (VBool (i1 <= i2))
evalBuiltin Gt exprs = do
  (i1, i2) <- expect2Ints exprs
  pure (VBool (i1 > i2))
evalBuiltin Ge exprs = do
  (i1, i2) <- expect2Ints exprs
  pure (VBool (i1 >= i2))
evalBuiltin Eq exprs = do
  expect2IntsOrBoolsOrAtoms exprs
    (\ i1 i2 -> pure (VBool (i1 == i2)))
    (\ b1 b2 -> pure (VBool (b1 == b2)))
    (\ x1 x2 -> pure (VBool (x1 == x2)))
evalBuiltin Ne exprs = do
  expect2IntsOrBoolsOrAtoms exprs
    (\ i1 i2 -> pure (VBool (i1 /= i2)))
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

doEvalTracing :: Bool -> Env -> Expr -> IO ()
doEvalTracing tracing env e =
  case runEval (withEnv env (eval e)) of
    (r, t) -> do
      when tracing (Text.putStr (renderEvalTrace t))
      case r of
        Left err -> print err
        Right x -> unless tracing (Text.putStrLn (render x))

doEvalDeclTracing :: Bool -> Env -> Decl -> IO (Env -> Env)
doEvalDeclTracing tracing env d =
  case runEval (withEnv env (evalDecl d)) of
    (r, t) -> do
      when tracing (Text.putStr (renderEvalTrace t))
      case r of
        Left err -> do
          print err
          pure id
        Right env' -> do
          pure (\ x -> extendEnv x env')

doEval :: Expr -> IO ()
doEval = doEvalTracing True emptyEnv

