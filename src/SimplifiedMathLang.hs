module SimplifiedMathLang where

import Base
import Simala.Eval.Type
import Simala.Expr.Evaluator
import Simala.Expr.Parser
import Simala.Expr.Type

import Text.Megaparsec

-- TODO: annotations, via fixed point or via directly embedding into the constructors?
-- TODO: mathlang as is doesn't have real folds, just flexible-arity functions

-- TODO:
--
-- Evaluation strategy? Make it lazy somehow to allow unknown values? Symbolic evaluation?
-- Builtins: if they can define their own evaluation strategy, then more things can be builtin: cons, list, if-then-else
--

exampleExpr :: Expr
exampleExpr =
  Builtin Sum [Lit (IntLit 2), Lit (IntLit 3)]

exampleExpr' :: Expr
exampleExpr' =
  Let Transparent "x" (Builtin Sum [Lit (IntLit 2), Lit (IntLit 3)]) (Builtin Product [Var "x", Var "x"])

exampleActions :: [EvalAction]
exampleActions =
  [Enter exampleExpr, Enter (Lit (IntLit 2)), Exit (VInt 2), Enter (Lit (IntLit 3)), Exit (VInt 3), Exit (VInt 5)]

revExample :: Expr
revExample =
  Fun Opaque ["xs"] (Builtin Fold [List [], Fun Opaque ["acc", "x"] (Cons (Var "x") (Var "acc")), Var "xs"])

revExample' :: Expr
revExample' =
  Let Opaque "rev" revExample (App (Var "rev") [List [Lit (IntLit 1), Lit (IntLit 2), Lit (IntLit 3)]])

-- >>> buildEvalTrace exampleActions
-- Trace (Builtin Sum [Lit (IntLit 2),Lit (IntLit 3)]) [Trace (Lit (IntLit 2)) [] (VInt 2),Trace (Lit (IntLit 3)) [] (VInt 3)] (VInt 5)

parseExpr :: Text -> IO ()
parseExpr e = parseTest (expr <* eof) e

parseAndEval :: Text -> IO ()
parseAndEval e =
  case parse (expr <* eof) "interactive" e of
    Right e' -> doEval e'
    Left err -> print err
