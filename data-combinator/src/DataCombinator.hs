module DataCombinator where

import Foundation
import Foundation.Collection

import qualified Analyze.Csv as Analyze
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.WAVE as Wave

import qualified DataCombinator.Csv as CsvCombinator
import qualified DataCombinator.Wav as WavCombinator

sensorFiles :: [String]
sensorFiles =
    [ "acc00"
    , "gyr00"
    , "mag00"
    , "prs00"
    , "tmp00"
    ]


soundFiles :: [String]
soundFiles =
    [ "vas01"
    , "vas02"
    , "vas03"
    , "vas04"
    , "vas05"
    , "vas06"
    ]

combineCSV :: String -> String -> IO ()
combineCSV datadir header = do
    let fileNames = fmap prependPath sensorFiles
    frames <- mapM Analyze.loadCSVFileWithHeader fileNames 
    combinedFrame <- CsvCombinator.combine $ nonEmpty_ frames
    LazyByteString.writeFile (prependPath "combined") $ Analyze.encodeWithHeader combinedFrame
  where
    prependPath file = toList ( datadir <> "/" <> header <> file <> ".csv")
  


combineWAV :: String -> String -> IO ()
combineWAV datadir header = do
    let fileNames = fmap prependPath soundFiles
    wavs <- mapM Wave.getWAVEFile fileNames
    combinedWav <- WavCombinator.combine $ nonEmpty_ wavs
    Wave.putWAVEFile (prependPath "combined") combinedWav
  where
    prependPath file = toList ( datadir <> "/" <> header <> file <> ".wav")