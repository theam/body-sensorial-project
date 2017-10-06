{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module DashboardServer.API where

import Control.Monad.IO.Class
import Data.Function
import Data.Text as Text
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Analyze
import Servant

import DashboardServer.Types

type DashboardAPI
     = "data"
     :> Capture "header" String
     :> Capture "start" Double
     :> Capture "end" Double
     :> Get '[JSON] (Vector DataSeries)
     :<|> "audio"
     :> Raw
     :<|> Raw

dashboardAPI :: Proxy DashboardAPI
dashboardAPI = Proxy

dashboardServer :: Server DashboardAPI
dashboardServer = dataHandler
    :<|> serveDirectoryFileServer "resources/data"
    :<|> serveDirectoryFileServer "static"


dataHandler :: String -> Double -> Double -> Handler (Vector DataSeries)
dataHandler header start end = do
    df <- liftIO $
        Analyze.loadCSVFileWithHeader $ "resources/data/" ++ header ++ "combined.csv"
    let dfKeys = Vector.take 9 $ Vector.drop 5 $ Analyze.rframeKeys df
    varElapsed <- liftIO $ Analyze.col "elapsed (s)" df
    let doubleElapsed = Vector.map (read . unpack) varElapsed :: Vector Double
    series <- liftIO $ Vector.mapM (serieColumn df doubleElapsed) dfKeys
    return $ series
           & getWindow start end
           & Vector.map (downsample downsampleFactor)
  where
    getWindow start' end' dataSeriesVector =
        Vector.map (adjustDataSeries start' end') dataSeriesVector
    adjustDataSeries start' end' (DataSeries n dps) =
        DataSeries
            n
            ( dps
            & Vector.takeWhile (\(elapsedSeconds:_) -> elapsedSeconds < end')
            & Vector.dropWhile (\(elapsedSeconds:_) -> elapsedSeconds < start')
            )
    serieColumn df doubleElapsed colName = do
        varCol <- Analyze.col colName df
        let doubleVariable =
                (Vector.map (read . unpack) varCol :: Vector Double)
        return $
            DataSeries
                (unpack colName)
                (Vector.zipWith (\a b -> [a, b]) doubleElapsed doubleVariable)

downsampleFactor :: Int
downsampleFactor = 5000

downsample :: Int -> DataSeries -> DataSeries
downsample threshold (DataSeries nm v) =
    DataSeries
        nm
        ( v
        & Vector.iterateN vLength (Vector.drop n)
        & Vector.takeWhile vectorNotNull
        & Vector.map Vector.head
        )
  where
    vLength = Vector.length v
    vectorNotNull = not . Vector.null
    n = vLength `div` threshold


