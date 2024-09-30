module Main where

import Base
import Simala.Expr.Type (emptyEnv, TraceMode (..))
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
  hspec $ forM_ exampleSimalaFiles $ \ inputFile -> do
    describe inputFile $
      it "compiles with correct output" $ do
        simalaGolden (dataDir </> "examples") inputFile
        

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
