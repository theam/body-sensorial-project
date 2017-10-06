{-# LANGUAGE DeriveGeneric #-}
module DashboardServer.Types where

import Data.Aeson
import Data.Vector as Vector
import GHC.Generics

type DataPoint = [Double]

data DataSeries = DataSeries
    { name :: String
    , dataPoints :: Vector DataPoint
    } deriving (Generic)
instance ToJSON DataSeries



