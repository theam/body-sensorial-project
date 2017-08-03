module Main where

import qualified Prelude
import Foundation

import Options.Applicative.Simple

import qualified DataCombinator

main :: IO ()
main = do
  (_, runCmd) <-
    simpleOptions
      "v1.0.0"
      "Patient data combinator"
      "Combines raw CSV and FLAC files into individual CSV and WAV files"
      (pure ()) $ do
        addCommand "csv"
                   "Combine multiple CSV files from a patient into a single one"
                   (combineCSV "../data")
                   (strOption (long "header"))
        addCommand "flac"
                   "Combine multiple FLAC files from a patient into a single WAV"
                   (combineFLAC "../data")
                   (strOption (long "header"))
  runCmd

combineCSV, combineFLAC :: Prelude.String -> Prelude.String -> IO ()
combineCSV dataDir filesHeader = DataCombinator.combineCSV (fromList dataDir) (fromList filesHeader)
combineFLAC dataDir filesHeader = DataCombinator.combineFLAC (fromList dataDir) (fromList filesHeader)
