{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as Text
import Data.Text (Text)

import Options.Generic

import DashboardServer.Main
import DataCombinator

data ApplicationArguments
    = Serve { port :: Int }
    | Combine { dataDirectory :: Text }
    deriving (Generic)

instance ParseRecord ApplicationArguments

main :: IO ()
main = do
    appArgs <- getRecord "BSP"
    runApp appArgs

runApp :: ApplicationArguments -> IO ()
runApp (Serve appPort) = runServer appPort
runApp (Combine appDataDir) = combineAll (Text.unpack appDataDir)

