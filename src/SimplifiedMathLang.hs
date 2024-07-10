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
  Fun Opaque ["xs"] (Builtin Foldl [Fun Opaque ["acc", "x"] (Cons (Var "x") (Var "acc")), List [], Var "xs"])

mapExample :: Expr
mapExample =
  Fun Opaque ["f", "xs"] (Builtin Foldr [Fun Opaque ["x", "r"] (Cons (App (Var "f") [Var "x"]) (Var "r")), List [], Var "xs"])

revExample' :: Expr
revExample' =
  Let Opaque "rev" revExample (App (Var "rev") [List [Lit (IntLit 1), Lit (IntLit 2), Lit (IntLit 3)]])

incListExample :: Expr
incListExample =
  Let Opaque "map" mapExample (App (Var "map") [Fun Transparent ["x"] (Builtin Sum [Var "x", Lit (IntLit 1)]), List [Lit (IntLit 1), Lit (IntLit 2), Lit (IntLit 3)]])

parseExpr :: Text -> IO ()
parseExpr e = parseTest (expr <* eof) e

parseAndEval :: Text -> IO ()
parseAndEval e =
  case parse (expr <* eof) "interactive" e of
    Right e' -> doEval e'
    Left err -> print err
