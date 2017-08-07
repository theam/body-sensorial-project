{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Debug.Trace
import GHC.Generics
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp
import Data.Aeson
import qualified Analyze
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Text (pack, unpack)
import Control.Monad.IO.Class
import Data.Function ((&))


downsampleFactor :: Int
downsampleFactor = 500

type DataAPI
  = "data"
  :> Capture "header" String
  :> Capture "start" Double
  :> Capture "end" Double
  :> Get '[JSON] (Vector DataSeries)

type DataPoint = [Double]

data DataSeries = DataSeries
    { name :: String
    , dataPoints :: Vector DataPoint
    } deriving (Generic)

instance ToJSON DataSeries

dataAPI :: Proxy DataAPI
dataAPI = Proxy


dataServer :: Server DataAPI
dataServer = dataHandler

downsample :: Int -> DataSeries -> DataSeries
downsample threshold (DataSeries nm v) = DataSeries nm
    ( v
    & Vector.iterateN vLength (Vector.drop n)
    & Vector.takeWhile vectorNotNull
    & Vector.map Vector.head
    )
  where
    vLength = Vector.length v
    vectorNotNull = not . Vector.null
    n = vLength `div` threshold


dataHandler :: String -> Double -> Double -> Handler (Vector DataSeries)
dataHandler header start end = do
    df <- liftIO $ Analyze.loadCSVFileWithHeader $ "../data/" ++ header ++ "combined.csv"
    let dfKeys = Vector.take 9 $ Vector.drop 4 $ Analyze.rframeKeys df
    varElapsed <- liftIO $ Analyze.col "elapsed" df
    let doubleElapsed = Vector.map (read . unpack) varElapsed :: Vector Double
    series <- liftIO $ Vector.mapM (serieColumn df doubleElapsed) dfKeys
    return $ Vector.map (downsample downsampleFactor) $ getWindow start end series
  where
      getWindow start end dataSeriesVector
        = Vector.map (adjustDataSeries start end) dataSeriesVector
      adjustDataSeries start end (DataSeries n dps) = DataSeries n
          ( dps
          & Vector.takeWhile (\(elapsedSeconds:_) -> elapsedSeconds < end)
          & Vector.dropWhile (\(elapsedSeconds:_) -> elapsedSeconds < start)
          )
      serieColumn df doubleElapsed colName = do
        varCol <- Analyze.col colName df
        let doubleVariable = (Vector.map (read . unpack) varCol :: Vector Double)
        return $ DataSeries
            (unpack colName)
            (Vector.zipWith (\a b -> [a,b]) doubleElapsed doubleVariable)

main :: IO ()
main = do
    putStrLn "Running server"
    run 8081 $ do
        simpleCors app
  where
    app = serve dataAPI dataServer
