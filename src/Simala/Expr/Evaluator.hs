module Simala.Expr.Evaluator where

import Base
import qualified Base.Map as Map
import Simala.Eval.Monad
import Simala.Eval.Type
import Simala.Expr.Type

eval :: Expr -> Eval Val
eval e = do
  enter e
  v <- eval' e
  exit v
  pure v

eval' :: Expr -> Eval Val
eval' (Builtin b exprs)  = evalBuiltin b exprs
eval' (Var x)            = look x
eval' Undefined          = crash
eval' (Lit l)            = evalLit l
eval' (List ls)          = do
  vs <- traverse eval ls
  pure (VList vs)
eval' (Cons e1 e2)       = do
  v1 <- eval e1
  v2 <- eval e2
  vs <- expectList v2
  pure (VList (v1 : vs))
eval' (App f args)       = do
  v <- eval f
  c <- expectFunction v
  evalClosure c args
eval' (Fun _t ns body)   = do
  env <- getEnv
  pure (VClosure (MkClosure ns body env))
eval' (Let _t n e1 e2)   = do
  env' <- bind [n] [e1]
  env <- getEnv
  withEnv (extendEnv env env') (eval e2)

evalClosure :: Closure -> [Expr] -> Eval Val
evalClosure (MkClosure argNames body env) args = do
  env' <- bind argNames args
  withEnv (extendEnv env env') (eval body)

evalClosureVal :: Closure -> [Val] -> Eval Val
evalClosureVal (MkClosure argNames body env) args = do
  env' <- bindVal argNames args
  withEnv (extendEnv env env') (eval body)

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

expect2IntsOrBools :: [Expr] -> (Int -> Int -> Eval a) -> (Bool -> Bool -> Eval a) -> Eval a
expect2IntsOrBools exprs ki kb = do
  (e1, e2) <- expectArity2 exprs
  v1 <- eval e1
  case v1 of
    VBool b1 -> do
      b2 <- (eval >=> expectBool) e2
      kb b1 b2
    VInt i1 -> do
      i2 <- (eval >=> expectInt) e2
      ki i1 i2
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
  expect2IntsOrBools exprs
    (\ i1 i2 -> pure (VBool (i1 == i2)))
    (\ b1 b2 -> pure (VBool (b1 == b2)))
evalBuiltin Ne exprs = do
  expect2IntsOrBools exprs
    (\ i1 i2 -> pure (VBool (i1 /= i2)))
    (\ b1 b2 -> pure (VBool (b1 /= b2)))
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

doEval :: Expr -> IO ()
doEval e =
  case runEval (eval e) of
    Left err -> print err
    Right t -> putStr (renderEvalTrace t)
