{-# LANGUAGE QuasiQuotes #-}
module DataCombinator.Numeric where

import Data.Monoid

import System.Process as System

combineCSV :: String -> String -> IO ()
combineCSV datadir header = System.callCommand $
    "python pybits/csvcombiner.py "
    <> datadir
    <> " "
    <> header

