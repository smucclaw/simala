module Main where

import Base
import qualified Base.Text as Text
import Simala.Expr.Evaluator
import Simala.Expr.Parser

import System.Environment

main :: IO ()
main = do
  inputFiles <- getArgs
  forM_ inputFiles $ \ f -> do
    e <- Text.readFile f
    case parseExpr f e of
      Right e' -> doEval e'
      Left err -> putStr err
