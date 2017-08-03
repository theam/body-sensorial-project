{-# LANGUAGE QuasiQuotes #-}
module DataCombinator where

import Foundation
import Foundation.Collection

import qualified Analyze
import qualified Data.ByteString.Lazy as LazyByteString
import qualified System.Process as System
import qualified Data.Vector as Vector
import H.Prelude
import H.Prelude.Interactive

sensorFiles :: [String]
sensorFiles =
    [ "gyr00"
    , "acc00"
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
    let (gyr : acc : mag : prs : tmp : []) = (toList . prependPath) <$> sensorFiles
    let output = (toList . prependPath) $ "combined"
    [r| require("data.table")

        gyr <- fread(gyr_hs)
        acc <- fread(acc_hs)
        mag <- fread(mag_hs)
        prs <- fread(prs_hs)
        tmp <- fread(tmp_hs)

        acc[, "x-axis (deg/s)":=numeric()]
        acc[, "y-axis (deg/s)":=numeric()]
        acc[, "z-axis (deg/s)":=numeric()]

        acc[, "x-axis (T)":=numeric()]
        acc[, "y-axis (T)":=numeric()]
        acc[, "z-axis (T)":=numeric()]

        acc[, "pressure (Pa)":=numeric()]

        acc[, "temperature (C)":=numeric()]

        total <- rbind(acc, gyr, mag, prs, tmp, fill=TRUE)

        names(total)[names(total) == "elapsed (s)"] = "elapsed"

        total <- total[order(elapsed)]

        repeatBefore = function (x) {
            ind = which(!is.na(x))
            if(is.na(x[1]))
                ind = c(1, ind)
            rep(x[ind], times = diff(c(ind, length(x) + 1)))
        }

        total <- sapply(total, repeatBefore)

        write.csv(x=total, file=output_hs, na="0")
    |]
    return ()
  where
    prependPath file = datadir <> "/" <> header <> file <> ".csv"



combineFLAC :: String -> String -> IO ()
combineFLAC datadir header = do
    let fileNames = prependPath <$> soundFiles
    mapM_ convertToWav fileNames
    combineWavs
    deleteWavs
  where
    prependPath file = datadir <> "/" <> header <> file <> ".flac"
    combineWavs = System.callCommand ( toList ("cd " <> datadir <> " && sox *.wav " <> header <> "combined.wav"))
    deleteWavs = System.callCommand ( toList ("cd " <> datadir <> " && rm -f *.flac.wav"))

convertToWav :: String -> IO ()
convertToWav file = System.callCommand ( toList ("ffmpeg -y -i \"" <> file <> "\" \"" <> file <> ".wav\"" ))
