module Simala.Repl where

import Base
import qualified Base.Text as Text
import Simala.Expr.Evaluator
import Simala.Expr.Parser
import Simala.Expr.Type

import System.Console.Haskeline

data ReplState =
  MkReplState
    { env     :: !Env
    , tracing :: !Bool
    }
  deriving stock (Generic, Show)

initialReplState :: ReplState
initialReplState =
  MkReplState emptyEnv False

repl :: IO ()
repl = do
  s <- newIORef initialReplState
  let
    go :: InputT IO ()
    go = do
      minput <- getInputLine "> "
      case minput of
        Nothing    -> pure () -- quit on end-of-file
        Just input ->
          case parseInstruction (Text.pack input) of
            Left err -> do
              liftIO (putStr err)
              go
            Right instr -> do
              handleInstruction s go instr
  runInputT defaultSettings go

handleInstruction :: IORef ReplState -> InputT IO () -> Instruction -> InputT IO ()
handleInstruction rs continue i = do
  s <- liftIO (readIORef rs)
  case i of
    Quit        -> pure ()
    ToggleTrace -> do
      liftIO (writeIORef rs (over #tracing not s))
      continue
    Declare d -> do
      f <- liftIO (doEvalDeclTracing s.tracing s.env d)
      liftIO (writeIORef rs (over #env f s))
      continue
    Eval e -> do
      liftIO (doEvalTracing s.tracing s.env e)
      continue
    Noop -> continue
