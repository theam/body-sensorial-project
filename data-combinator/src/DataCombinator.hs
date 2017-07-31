module DataCombinator where

import Foundation
import Foundation.Collection

import qualified Analyze.Csv as Analyze
import qualified Data.ByteString.Lazy as LazyByteString

import qualified DataCombinator.Csv as CsvCombinator

sensorFiles :: [String]
sensorFiles =
    [ "acc00"
    , "gyr00"
    , "mag00"
    , "prs00"
    , "tmp00"
    ]

combineCSV :: String -> String -> IO ()
combineCSV datadir header = do
    let fileNames = fmap prependPath sensorFiles
    frames <- mapM Analyze.loadCSVFileWithHeader fileNames 
    combinedFrame <- CsvCombinator.combine $ nonEmpty_ frames
    LazyByteString.writeFile (prependPath "combined") $ Analyze.encodeWithHeader combinedFrame
  where
    prependPath file = toList ( datadir <> "/" <> header <> file <> ".csv")
  


combineFLAC :: String -> String -> IO ()
combineFLAC datadir header = putStrLn header