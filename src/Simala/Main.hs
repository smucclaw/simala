module Simala.Main where

import Base
import Simala.Json.Parser
import Simala.Expr.Evaluator
import Simala.Expr.Parser
import Simala.Expr.Type
import Simala.Repl
import Simala.TranspileToLam4

import qualified Data.Aeson as Aeson
import qualified Base.Text as Text
import Options.Applicative as Options

data Options =
  MkOptions
    { mode    :: !Mode
    , tracing :: TraceMode
    , files   :: [FilePath]
    }

data Mode =
  Evaluate | Repl | TranspileToLam4
  deriving stock Eq

optionsDescription :: Options.Parser Options
optionsDescription =
  MkOptions
  <$> (    flag' Repl (long "repl" <> help "Start in REPL mode")
       <|> flag' TranspileToLam4 (long "lam4" <> help "Transpile to Lam4")
       <|> pure Evaluate
      )
  <*> (toTracingMode <$> strOption (long "tracing" <> help "Tracing, one of \"off\", \"full\" (default), \"results\"") <|> pure TraceResults)
  <*> many (strArgument (metavar "FILES..."))

toTracingMode :: String -> TraceMode
toTracingMode "full"    = TraceFull
toTracingMode "results" = TraceResults
toTracingMode "off"     = TraceOff
toTracingMode _         = TraceFull

optionsConfig :: Options.ParserInfo Options
optionsConfig =
  info (optionsDescription <**> helper)
    (  fullDesc
    <> header "simala (Simplified Math Lang, for use in L4)"
    )

main :: IO ()
main = do
  options <- Options.execParser optionsConfig
  if null options.files && options.mode /= Repl
    then do
      hPutStrLn stderr "simala: no input files given; use --help for help"
    else
      case options.mode of
        Repl -> do
          env <- compileDeclOrJsonFiles options.tracing options.files emptyEnv
          runRepl env
        Evaluate ->
          compileFiles options.tracing options.files emptyEnv
        TranspileToLam4 ->
          mapM_ transpileToLam4File options.files

-- | Try to opportunistically parse as JSON, otherwise as Simala.
compileDeclOrJsonFile :: TraceMode -> FilePath -> Env -> IO Env
compileDeclOrJsonFile tm inputFile env = do
  aesonResult <- Aeson.eitherDecodeFileStrict inputFile
  case aesonResult of
    Right v -> case jsonToDecls v of
      Just ds -> doEvalDeclsTracing tm env ds
      Nothing -> do
        putStr ("Input file " <> inputFile <> " looks like JSON, but does not contain a record with declarations.")
        pure emptyEnv
    Left _ -> compileDeclFile tm inputFile env

compileDeclFile :: TraceMode -> FilePath -> Env -> IO Env
compileDeclFile tm inputFile env = do
  input <- Text.readFile inputFile
  case parseDecls inputFile input of
    Right ds -> doEvalDeclsTracing tm env ds
    Left err -> putStr err >> pure env

transpileToLam4File :: FilePath -> IO ()
transpileToLam4File inputFile = do
  input <- Text.readFile inputFile
  case parseDecls inputFile input of
    Right ds -> Text.putStr (doToLam4 ds)
    Left err -> putStr err

compileDeclOrJsonFiles :: TraceMode -> [FilePath] -> Env -> IO Env
compileDeclOrJsonFiles _  []       env = pure env
compileDeclOrJsonFiles tm (f : fs) env = do
  env' <- compileDeclOrJsonFile tm f env
  compileDeclOrJsonFiles tm fs env'

compileFiles :: TraceMode -> [FilePath] -> Env -> IO ()
compileFiles _  []       _   = pure ()
compileFiles tm (f : fs) env = do
  env' <- compileDeclOrJsonFile tm f env
  compileFiles tm fs env'
