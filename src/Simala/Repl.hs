module Simala.Repl (runRepl) where

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

newtype Repl a =
  MkRepl { run :: IORef ReplState -> InputT IO a }
  deriving
    (Functor, Applicative, Monad, MonadReader (IORef ReplState), MonadIO)
    via ReaderT (IORef ReplState) (InputT IO)

instance MonadState ReplState Repl where
  get   = ask >>= liftIO . readIORef
  put s = ask >>= liftIO . flip writeIORef s

inputLine :: String -> Repl (Maybe Text)
inputLine prompt =
  MkRepl (const ((Text.pack <$>) <$> getInputLine prompt))

toggleTracing :: Repl ()
toggleTracing =
  modifying' #tracing not

getTracing :: Repl Bool
getTracing =
  use #tracing

getEnv :: Repl Env
getEnv =
  use #env

setEnv :: Env -> Repl ()
setEnv =
  assign' #env

initialReplState :: ReplState
initialReplState =
  MkReplState emptyEnv False

-- | Entry point for the repl.
runRepl :: IO ()
runRepl = do
  s <- newIORef initialReplState
  runInputT defaultSettings (repl.run s)

repl :: Repl ()
repl = do
  minput <- inputLine "> "
  case minput of
    Nothing    -> pure () -- quit on end-of-file
    Just input ->
      case parseInstruction input of
        Left err    -> do
          -- parse errors are printed, and we continue the loop
          liftIO (putStr err)
          repl
        Right instr ->
          -- instructions can quit,
          -- so we pass the continuation as an argument
          handleInstruction repl instr

handleInstruction :: Repl () -> Instruction -> Repl ()
handleInstruction continue i = do
  case i of
    ReplCommand Quit -> pure ()
    ReplCommand Help -> do
      liftIO $ do
        putStrLn "Commands available:\n"
        putStrLn "  <identifier> = <expr>      bind result of <expr> to <identifier>"
        putStrLn "  rec <identifier> = <expr>  recursive binding"
        putStrLn "  <expr>                     evaluate <expr>"
        putStrLn "  :t, :trace                 toggle execution traces"
        putStrLn "  :h, :help                  this help text"
        putStrLn "  :q, :quit                  quit"
      continue
    ReplCommand ToggleTrace -> do
      toggleTracing
      continue
    Declare d -> do
      t <- getTracing
      env <- getEnv
      f <- liftIO (doEvalDeclTracing t env d)
      setEnv (f env)
      continue
    Eval e -> do
      t <- getTracing
      env <- getEnv
      liftIO (doEvalTracing t env e)
      continue
    Noop -> continue
