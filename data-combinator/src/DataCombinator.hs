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
    [r|
        require("data.table")
        require("zoo")

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

        total <- na.locf(total)

        NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

        nm <- names(total)[colSums(is.na(total)) != 0]

        total[, (nm) := lapply(nm, function(x) {
            x <- get(x)
            x[is.na(x)] <- mean(x, na.rm = TRUE)
            x
        })]

        x <- sapply(total[, 3:14], NA2mean)

        write.csv(x=total, file="/tmp/tempdata.csv", quote=FALSE)
    |]
    tempdata <-
         Analyze.loadCSVFileWithHeader "/tmp/tempdata.csv"
    let newRF = Analyze.filter isComplete tempdata
    let out = Analyze.encodeWithoutHeader newRF
    let outH = Analyze.encodeWithHeader newRF
    LazyByteString.writeFile output outH
    LazyByteString.appendFile output out
  where
    prependPath file = datadir <> "/" <> header <> file <> ".csv"
    isComplete _ _ _ els = not $ "NA" `Vector.elem` els



combineFLAC :: String -> String -> IO ()
combineFLAC datadir header = do
    let fileNames = prependPath <$> soundFiles
    mapM_ convertToWav fileNames
    combineWavs
    deleteFlacWavs
  where
    prependPath file = datadir <> "/" <> header <> file <> ".flac"
    combineWavs = System.callCommand ( toList ("cd " <> datadir <> " && sox "<>header<>"*.wav " <> header <> "combined.wav rate 16k"))
    deleteWavs = System.callCommand ( toList ("cd " <> datadir <> " && rm -f *.wav"))
    deleteFlacWavs = System.callCommand ( toList ("cd " <> datadir <> " && rm -f *.flac.wav"))

convertToWav :: String -> IO ()
convertToWav file = System.callCommand ( toList ("ffmpeg -y -i \"" <> file <> "\" \"" <> file <> ".wav\"" ))
