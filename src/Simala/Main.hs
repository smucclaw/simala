module Simala.Main where

import Base
import Simala.Json.Parser
import Simala.Expr.Evaluator
import Simala.Expr.Parser
import Simala.Expr.Type
import Simala.Repl

import qualified Data.Aeson as Aeson
import qualified Base.Text as Text
import Options.Applicative as Options

data Options =
  MkOptions
    { repl    :: !Bool
    , tracing :: TraceMode
    , files   :: [FilePath]
    }

optionsDescription :: Options.Parser Options
optionsDescription =
  MkOptions
  <$> switch (long "repl" <> help "Start in REPL mode")
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
  if null options.files && not options.repl
    then do
      hPutStrLn stderr "simala: no input files given; use --help for help"
    else if options.repl
      then do
        env <- compileDeclOrJsonFiles options.tracing options.files emptyEnv
        runRepl env
      else compileFiles options.tracing options.files emptyEnv

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
