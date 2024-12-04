module Main where

import Base
import qualified Base.Text as Text
import Simala.Expr.Type (emptyEnv, TraceMode (..))
import qualified Simala.Expr.Parser as Parser
import qualified Simala.Expr.ExactPrint as Simala
import Simala.Main

import System.FilePath
import System.FilePath.Glob
import System.IO.Silently
import Test.Hspec
import Test.Hspec.Golden

import Paths_simala

main :: IO ()
main = do
  dataDir <- getDataDir
  exampleSimalaFiles <- globDir1 (compile "*.simala") (dataDir </> "examples")
  hspec $ do
    forM_ exampleSimalaFiles $ \ inputFile -> do
      describe inputFile $
        it "compiles with correct output" $ do
          simalaGolden (dataDir </> "examples") inputFile

    forM_ exampleSimalaFiles $ \ inputFile -> do
      describe inputFile $
        it "exactprints" $ do
          simalaExactPrintGolden (dataDir </> "examples") inputFile


simalaGolden :: String -> String -> IO (Golden String)
simalaGolden dir inputFile = do
  firstLine <- take 1 . lines <$> readFile inputFile
  let
    extraFiles =
      case firstLine of
        []                        -> []
        [l] | take 5 l == "-- ! " -> ((dir </>) <$> words (drop 5 l))
        _                         -> []
  (output_, _) <- capture (compileDeclOrJsonFiles TraceResults (extraFiles ++ [inputFile]) emptyEnv)
  pure
    Golden
      { output = output_
      , encodePretty = show
      , writeToFile = writeFile
      , readFromFile = readFile
      , goldenFile = dir </> "tests" </> (takeFileName inputFile -<.> "golden")
      , actualFile = Just (dir </> "tests" </> (takeFileName inputFile -<.> "actual"))
      , failFirstTime = False
      }

simalaExactPrintGolden :: String -> String -> IO (Golden Text)
simalaExactPrintGolden dir inputFile = do
  input <- Text.readFile inputFile
  let output_ = case Parser.parseDecls inputFile input of
        Left err -> Text.pack err
        Right ds -> Simala.exactprint ds
  pure
    Golden
      { output = output_
      , encodePretty = Text.unpack
      , writeToFile = Text.writeFile
      , readFromFile = Text.readFile
      , goldenFile = dir </> "tests" </> (takeFileName inputFile -<.> "ep.golden")
      , actualFile = Just (dir </> "tests" </> (takeFileName inputFile -<.> "ep.actual"))
      , failFirstTime = False
      }
