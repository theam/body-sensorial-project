{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Analyze
import Control.Monad.IO.Class
import Data.Aeson
import Data.List.Split
import Codec.Wav
import Numeric.Transform.Fourier.FFT
import Data.Complex
import Data.Audio
import Data.Array.IArray
import qualified Data.Array as Array
import Data.Monoid
import Data.Int
import Data.Function ((&))
import Data.Text (pack, unpack)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Debug.Trace
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Servant.API

downsampleFactor :: Int
downsampleFactor = 5000

type DataAPI
     = "data"
     :> Capture "header" String
     :> Capture "start" Double
     :> Capture "end" Double
     :> Get '[ JSON] (Vector DataSeries)
     :<|> "spectrum"
     :> Capture "header" String
     :> Get '[JSON] (Vector (Vector (Vector Double)))
     :<|> Raw

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
    :<|> spectrumHandler
    :<|> serveDirectoryFileServer "static"

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


spectrumHandler :: String -> Handler (Vector (Vector (Vector Double)))
spectrumHandler header = do
    Right snd <- liftIO . importFile $ "../data/" <> header <> "combined.wav" :: Handler (Either String (Audio Int16))
    let x = sampleData snd
          & elems
          & chunksOf 1024
          & fmap (Array.listArray (0,1023) . fmap toCpx)
          & fmap fft
          & fmap Array.elems
          & fmap (\x -> Vector.fromList $ fmap (\y -> Vector.fromList [realPart y, imagPart y]) x)
          & Vector.fromList
    return x
  where
    toCpx x = fromIntegral x :+ 0.0 :: Complex Double


dataHandler :: String -> Double -> Double -> Handler (Vector DataSeries)
dataHandler header start end = do
    df <- liftIO $
        Analyze.loadCSVFileWithHeader $ "../data/" ++ header ++ "combined.csv"
    let dfKeys = Vector.take 9 $ Vector.drop 4 $ Analyze.rframeKeys df
    varElapsed <- liftIO $ Analyze.col "elapsed" df
    let doubleElapsed = Vector.map (read . unpack) varElapsed :: Vector Double
    series <- liftIO $ Vector.mapM (serieColumn df doubleElapsed) dfKeys
    return $ series
           & getWindow start end
           & Vector.map (downsample downsampleFactor)
  where
    getWindow start end dataSeriesVector =
        Vector.map (adjustDataSeries start end) dataSeriesVector
    adjustDataSeries start end (DataSeries n dps) =
        DataSeries
            n
            ( dps
            & Vector.takeWhile (\(elapsedSeconds:_) -> elapsedSeconds < end)
            & Vector.dropWhile (\(elapsedSeconds:_) -> elapsedSeconds < start)
            )
    serieColumn df doubleElapsed colName = do
        varCol <- Analyze.col colName df
        let doubleVariable =
                (Vector.map (read . unpack) varCol :: Vector Double)
        return $
            DataSeries
                (unpack colName)
                (Vector.zipWith (\a b -> [a, b]) doubleElapsed doubleVariable)

main :: IO ()
main = do
    putStrLn "Running server"
    run 8081 app
  where
    app = serve dataAPI dataServer
