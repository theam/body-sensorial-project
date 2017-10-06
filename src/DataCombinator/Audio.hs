module DataCombinator.Audio where

import Data.Monoid

import System.Process as System


soundFiles :: [String]
soundFiles =
    [ "vas01"
    , "vas02"
    , "vas03"
    , "vas04"
    , "vas05"
    , "vas06"
    ]


combineFLAC :: String -> String -> IO ()
combineFLAC datadir header = do
    let fileNames = prependPath <$> soundFiles
    mapM_ convertToWav fileNames
    combineWavs
    deleteFlacWavs
  where
    prependPath file =
        datadir <> "/" <> header <> file <> ".flac"
    combineWavs = System.callCommand $
        "cd "
        <> datadir
        <> " && sox "
        <> header
        <> "*.wav "
        <> header
        <> "combined.wav rate 16k"
    deleteFlacWavs = System.callCommand $
        "cd " <> datadir <> " && rm -f *.flac.wav"


convertToWav :: String -> IO ()
convertToWav file = System.callCommand $
    "ffmpeg -y -i \""
    <> file
    <> "\" \""
    <> file
    <> ".wav\""
