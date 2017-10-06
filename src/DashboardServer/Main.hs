module DashboardServer.Main where

import Network.Wai.Handler.Warp

import Servant

import DashboardServer.API


runServer :: Int -> IO ()
runServer port = do
    putStrLn $ "Running server on port " ++ show port
    run port app
  where
    app = serve dashboardAPI dashboardServer
