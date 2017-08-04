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


type DataAPI
  = "data"
  :> Capture "header" String
  :> Capture "variable" String
  :> Capture "start" Int
  :> Capture "end" Int
  :> Get '[JSON] (Vector DataPoint)

data DataPoint = DataPoint
  { x :: Double
  , y :: Double
  } deriving Generic

instance ToJSON DataPoint

dataAPI :: Proxy DataAPI
dataAPI = Proxy


dataServer :: Server DataAPI
dataServer = dataHandler



dataHandler :: String -> String -> Int -> Int -> Handler (Vector DataPoint)
dataHandler header variable start end = do
    dbug $ "Got request for header " ++ header ++ " and variable " ++ variable
    dbug "Loading CSV"
    df <- liftIO $ Analyze.loadCSVFileWithHeader $ "../data/" ++ header ++ "combined.csv"
    dbug "Getting elapsed time"
    varElapsed <- liftIO $ Analyze.col "elapsed" df
    dbug "Getting variable"
    varCol <- liftIO $ Analyze.col variable' df
    doubleElapsed <- do
        dbug "Converting elapsed to double"
        return ( Vector.map (read . traceId . unpack) varElapsed :: Vector Double )
    doubleVariable <- do
        dbug "Converting variable to double"
        return ( Vector.map (read . traceId . unpack) varCol :: Vector Double )
    dbug "Zipping and returning"
    return $ Vector.zipWith DataPoint doubleElapsed doubleVariable
  where
    variable' = pack variable
    dbug = liftIO . putStrLn


f :: Double -> Double
f x = (a * x^2) + b * x + c
  where
    a = 1.0
    b = 1.0
    c = (-2.0)


main :: IO ()
main = do
    putStrLn "Running server"
    run 8081 $ do
        simpleCors app
  where
    app = serve dataAPI dataServer
