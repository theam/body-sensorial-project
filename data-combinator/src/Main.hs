module Main where

import qualified Prelude
import Foundation
import qualified Data.Text
import Data.Text (Text)
import Data.Function ((&))
import qualified Data.Text as Text

import Options.Applicative.Simple
import System.Directory

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
        addCommand "all"
                   "Check for available headers and run combine csv and flac for them all"
                   (const $ combineAll)
                   ( pure () )
  runCmd

combineCSV, combineFLAC :: Prelude.String -> Prelude.String -> IO ()
combineCSV dataDir filesHeader = DataCombinator.combineCSV (fromList dataDir) (fromList filesHeader)
combineFLAC dataDir filesHeader = DataCombinator.combineFLAC (fromList dataDir) (fromList filesHeader)

combineAll :: IO ()
combineAll = do
    files <- listDirectory $ fromList "../data"
    let headers = files
                & fmap (getPrefix . Text.pack)
                & filter (/= "")
                & fmap Text.unpack
    Prelude.mapM_ (combineCSV "../data") headers
    Prelude.mapM_ (combineFLAC "../data") headers
  where
    getPrefix t
        = case Text.breakOn "acc" t of
            (x , "acc00.csv") -> x
            (_, "") -> ""
