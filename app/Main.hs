module Main where

import Base
import qualified Base.Text as Text
import Simala.Expr.Evaluator
import Simala.Expr.Parser
import Simala.Expr.Type
import Simala.Repl

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
  <*> (toTracingMode <$> strOption (long "tracing" <> help "Tracing, one of \"off\", \"full\" (default), \"results\"") <|> pure TraceFull)
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
      then runRepl
      else compileFiles options.tracing options.files

compileFiles :: TraceMode -> [FilePath] -> IO ()
compileFiles tm inputFiles = do
  forM_ inputFiles $ \ f -> do
    e <- Text.readFile f
    case parseExpr f e of
      Right e' -> doEval tm e'
      Left err -> putStr err
